(*
   OCaml implementation of realpath.

   Remove once we depend on ocaml >= 4.13? Or just reduce
   the code to a simple calls to Unix.realpath.
   Unfortunately there is no unixcompat like we have stdcompat.

   related functions:
    - Unix.realpath(), but available only since OCaml 4.13
    - Common.fullpath(), but not as good as Realpath
    - efuns/c/realpath.ml (an OCaml binding to the C library realpath(3))
      but Unix.realpath() should be the same
*)
open Printf

(**********************************************************)
(* Helpers *)
(**********************************************************)

let ( / ) = Fpath.( / )
let max_symlink_count = 1000

let check_dir path =
  let path_str = Fpath.to_string path in
  (* important: Unix.stat resolves symlinks *)
  match (Unix.stat path_str).st_kind with
  | S_DIR -> ()
  | _ -> failwith ("not a directory: " ^ path_str)

let make_root ~vol = vol ^ String.concat Fpath.dir_sep [ ""; "" ] |> Fpath.v

(**********************************************************)
(* Entry point *)
(**********************************************************)

(*
   relative path: a path relative to a directory.
   absolute path: a path relative to a root (on Windows, there are multiple
                  roots but just one on Unix systems).
   real path: the normalized, absolute path of a file that doesn't contain
              "." or ".." as one of its segments and doesn't have a symbolic
              link as one of its prefixes.
*)
let realpath path =
  let symlink_count = ref 0 in
  let rec resolve cwd path =
    (* cwd is the real path used to resolve the path if it's relative. *)
    let real_parent, segs =
      if Fpath.is_rel path then
        (* We delay the evaluation of the current folder until here
           because it may fail, especially during the tests where we delete
           and re-create the work folder over and over again. *)
        (Lazy.force cwd, Fpath.segs path)
      else
        let vol, abs_path = Fpath.split_volume path in
        match Fpath.segs abs_path with
        | [ ""; "" ] ->
            (* / *)
            (path, [])
        | "" :: segs ->
            (* /foo/bar *)
            (make_root ~vol, segs)
        | [] -> assert false
        | _ :: _ -> assert false
    in
    resolve_segments real_parent segs
  and resolve_segments real_parent segs =
    match segs with
    | [] -> real_parent
    | "" :: segs -> resolve_segments real_parent segs
    | "." :: segs ->
        check_dir real_parent;
        resolve_segments real_parent segs
    | ".." :: segs ->
        check_dir real_parent;
        (* note that the parent of the root is the root itself *)
        resolve_segments (Fpath.parent real_parent |> Fpath.rem_empty_seg) segs
    | seg :: segs ->
        let p = real_parent / seg in
        let p = resolve_symlink real_parent p in
        resolve_segments p segs
  (* Return a real path from an absolute path whose last segment
     may be a symbolic link. *)
  and resolve_symlink real_parent abs_path =
    (* 'path' is an absolute 'path' on volume 'vol'. Its parent is
       fully resolved and given by 'real_parent'.
       'path' may be a symbolic link to any path. *)
    let abs_path_str = Fpath.to_string abs_path in
    match (Unix.lstat abs_path_str).st_kind with
    | S_LNK ->
        incr symlink_count;
        if !symlink_count > max_symlink_count then
          failwith
            ("too many symbolic link indirections: " ^ Fpath.to_string abs_path);
        resolve (lazy real_parent) (Unix.readlink abs_path_str |> Fpath.v)
    | _ -> abs_path
  in
  resolve (lazy (Unix.getcwd () |> Fpath.v)) path
[@@profiling]

let realpath_str s =
  match Fpath.of_string s with
  | Ok path -> realpath path |> Fpath.to_string
  | Error (`Msg msg) -> invalid_arg ("Realpath.realpath_str: " ^ msg)

(**********************************************************)
(* Tests (Unix only) *)
(**********************************************************)

let test () =
  let check (input, expected_output) =
    Alcotest.(check string)
      ("realpath " ^ input) expected_output (realpath_str input)
  in
  let fails input =
    try
      let res = realpath_str input in
      Alcotest.fail (sprintf "realpath %s: incorrectly returned %s" input res)
    with
    | _ -> ()
  in
  Testutil_files.with_tempfiles ~chdir:true
    [
      Symlink ("loop_a", "loop_b");
      Symlink ("loop_b", "loop_a");
      (* Previously we used /tmp for testing, but in OSX /tmp is a symlink to
       * /private/tmp. After that we used /bin for testing, but on Fedora, /bin
       * is a symlink to /usr/bin. *)
      Symlink ("link-to-usr", "/usr");
      File ("regfile", "");
      Dir ("sub", [ Symlink ("link-to-reg", "..//regfile///") ]);
    ]
    (fun root ->
      let root_s =
        Fpath.to_string root
        (* In OSX `root` is /var/... and at the same time /var is a symlink
         * to /private/var, so we need to resolve the symlink here or else all
         * subsequent tests based on this `root` will fail. *)
        |> realpath_str
      in
      List.iter check
        [
          ("/", "/");
          ("/usr", "/usr");
          ("/usr/.", "/usr");
          ("/usr/..", "/");
          ("/usr/", "/usr");
          (root_s, root_s);
          (* not sure why Fpath considers an extra leading slash to be
             a volume, which we preserve like a Windows volume. *)
          ("//usr", "//usr");
          ("/////usr", "//usr");
          (sprintf "%s//sub" root_s, sprintf "%s/sub" root_s);
          (sprintf "%s/sub/link-to-reg" root_s, sprintf "%s/regfile" root_s);
        ];
      List.iter fails
        [
          sprintf "%s/loop_a" root_s;
          sprintf "%s/xxxxxxxxxxxx" root_s;
          sprintf "%s/sub/regfile/." root_s;
          sprintf "%s/sub/regfile/.." root_s;
        ];
      Testutil_files.with_chdir (Fpath.v "sub") (fun _cwd ->
          List.iter check
            [
              (".", sprintf "%s/sub" root_s);
              ("..", root_s);
              ("../link-to-usr", "/usr");
            ]))

let () =
  match Sys.os_type with
  | "Unix" -> Testutil.test "realpath" test
  | _ ->
      (* no tests for Windows *)
      ()
