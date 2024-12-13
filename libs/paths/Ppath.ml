(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Abstract type for a file path within a project

   The name of the module imitates Fpath.ml, but use Ppath.ml for
   Project path (instead of File path).

   !!! The tests for this module are in the git_wrapper library because
   they depend on git.
   TODO: create a 'project' library that depends both on 'git_wrapper'
   and 'paths'?
*)

open Common
open Fpath_.Operators

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
   Type to represent an *absolute*, *normalized* path relative to a project
   root. This is purely syntactic. For example,

     in_project ~root:(Fpath.v "/a") (Fpath.v "/a/b/c")

   will return { segments = [""; "b"; "c"]; string = "/b/c"; }
 *)
type t = {
  (* Path segments within the project root.
   * Invariants:
     - the first element of the list should always be "",
       because all ppaths are absolute paths.
     - no segment may be "." or "..".
     - no segment may contain a "/".
   *)
  segments : string list;
  (* The string representation of the ppath as used for matching file paths
     with gitignore and semgrepignore implementation.
     This path uses '/' as a separator regardless of the platform
     and starts with one. *)
  string : string;
}
[@@deriving show]

let string_of_segments segments = String.concat "/" segments

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* Fast access to the string representation is needed when matching a ppath
   against many gitignore or semgrepignore patterns because they use regexps
   on the string rather than working on string segments individually. *)
let to_string_fast path = path.string

(* TODO: make a rel_segments function so the caller does not have to do
   let rel_segments =
      match Ppath.segments full_git_path with
      | "" :: xs -> xs
      | __else__ -> assert false
    in
*)

let segments x = x.segments

(* Return the list of segments of a ppath without the leading slash. *)
let relative_segments (ppath : t) =
  match ppath.segments with
  | [] -> assert false
  | [ "" ] -> assert false
  | "" :: segs -> segs
  | _ -> assert false

(*****************************************************************************)
(* Builder helpers (not exposed in Ppath.mli) *)
(*****************************************************************************)

let rec normalize_aux (xs : string list) : string list =
  match xs with
  | ".." :: xs -> ".." :: normalize_aux xs
  | [ "" ] as xs (* preserve trailing slash *) -> xs
  | ("." | "") :: xs -> normalize_aux xs
  | _ :: ".." :: xs -> normalize_aux xs
  | x :: xs as orig ->
      let res = normalize_aux xs in
      (* If nothing changes via normalization, return the original list *)
      if Stdlib.( == ) res xs then orig
      else (* Something changed, make another pass *)
        normalize_aux (x :: res)
  | [] -> []

let normalize_segments (segments : string list) =
  match segments with
  | "" :: xs -> (
      match normalize_aux xs with
      | ".." :: _ -> invalid_arg ("invalid ppath: " ^ String.concat "/" segments)
      | [] -> [ ""; "" ]
      | segments -> "" :: segments)
  | _ ->
      invalid_arg
        ("Ppath.create: not an absolute ppath: " ^ String.concat "/" segments)

let check_normalized_segment str =
  if String.contains str '/' then
    invalid_arg ("Ppath.create: path segment may not contain a slash: " ^ str)
  else
    match str with
    | "."
    | ".." ->
        invalid_arg ("Ppath.create: unsupported path segment: " ^ str)
    | _ -> ()

let check_nonempty_normalized_segment str =
  match str with
  | "" -> invalid_arg "Ppath.create: misplaced empty segment"
  | _ -> check_normalized_segment str

let check_normalized_segments segments =
  let rec iter segs =
    match segs with
    | [ "" ] (* trailing slash *) -> ()
    | seg :: segs ->
        check_nonempty_normalized_segment seg;
        iter segs
    | [] -> ()
  in
  match segments with
  | []
  | [ _ ] ->
      invalid_arg
        ("Ppath.create: ppath should have at least 2 segments: "
       ^ String.concat "/" segments)
  | "" :: segs -> iter segs
  | _ ->
      invalid_arg
        ("Ppath.create: ppath must be absolute (start with '/'): "
       ^ String.concat "/" segments)

let unsafe_create segments = { segments; string = string_of_segments segments }

let create segments =
  let norm_segments = normalize_segments segments in
  check_normalized_segments norm_segments;
  unsafe_create norm_segments

let root = create [ ""; "" ]

(*****************************************************************************)
(* Append *)
(*****************************************************************************)

(* use same terminology as in Fpath *)
let add_seg path seg = create (path.segments @ [ seg ])

(* saving you 3 neurons *)
let add_segs (path : t) segs = create (path.segments @ segs)

let append_fpath (base : t) fpath =
  match Fpath.segs fpath with
  | "" :: _ ->
      invalid_arg ("Ppath.append_fpath: not a relative path: " ^ !!fpath)
  | segs -> create (base.segments @ segs)

module Operators = struct
  let ( / ) = add_seg
end

(*****************************************************************************)
(* Export *)
(*****************************************************************************)

let to_fpath ?root path =
  match path.segments with
  | "" :: (seg :: other_segments as segments) ->
      let root, segments =
        match (root, seg) with
        | None, "" -> (* '/' -> '.' *) (Fpath.v ".", [])
        | Some root, "" when Fpath.is_current_dir root ->
            (* '.' + '/' -> '.' *)
            (Fpath.v ".", [])
        | None, seg -> (Fpath.v seg, other_segments)
        | Some root, seg when Fpath.is_current_dir root ->
            (* If the project root is "." and the ppath is "/a",
               produce "a" rather than "./a".
               However, if the project root is "./b", then produce "./b/a". *)
            (Fpath.v seg, other_segments)
        | Some root, _ -> (root, segments)
      in
      List.fold_left Fpath.add_seg root segments
  | _ -> assert false

(*
   Represent Ppaths as strings using '/' as the separator regardless
   of the platform. These are not valid file system paths in general.
   They may be used only in tests and in internal assertions
   (assert or invalid_arg).
*)
let of_string_for_tests string = create (String.split_on_char '/' string)

(* Show the ppath in string form. Doesn't need to be fast.
   We use a different name to distinguish use cases more clearly. *)
let to_string_for_tests = to_string_fast

let relativize ~root:orig_root orig_ppath =
  let rec aux root ppath =
    match (root, ppath) with
    | [ "" ], [ "" ] -> Fpath.v "."
    | [ "" ], [] -> (* tolerate "/foo/" vs "/foo" *) Fpath.v "."
    | [ "" ], segs -> Fpath_.of_relative_segments segs
    | [], [] -> Fpath.v "."
    | [], [ "" ] -> (* prefer "." over "./" *) Fpath.v "."
    | [], segs -> Fpath_.of_relative_segments segs
    | _ :: _, [] ->
        invalid_arg
          (spf "Ppath.relativize: %S is shorter than %S"
             (to_string_for_tests orig_root)
             (to_string_for_tests orig_ppath))
    | x :: xs, y :: ys ->
        if x = y then aux xs ys
        else
          invalid_arg
            (spf "Ppath.relativize: %S is not a prefix of %S"
               (to_string_for_tests orig_root)
               (to_string_for_tests orig_ppath))
  in
  aux (relative_segments orig_root) (relative_segments orig_ppath)

(*****************************************************************************)
(* Project Builder *)
(*****************************************************************************)

(*
   Prepend "./" to relative paths so as to make "." a prefix.
*)
let make_matchable_relative_path path =
  match Fpath.segs path with
  | "" :: _ -> (* absolute *) path
  | "." :: _ -> (* keep as is *) path
  | _rel -> Fpath.v "." // path

(*
   This is a collection of fixes on top of Fpath.rem_prefix to make it
   work as a user would expect.

   if 'root' is a parent of 'path', then return the relative path
   to go from the root to that path:

     (/a, /a/b) -> b

   We try to make it work even if the root string is not a string prefix
   of the path e.g.

     (., a) -> a
     (./a, a/b) -> b
     (a, ./a/b) -> b

   This returns a relative path.

   TODO: Move to File module?
*)
let remove_prefix root path =
  let had_a_trailing_slash = Fpath.is_dir_path path in
  (* normalize paths syntactically e.g. "./a/b/c/.." -> "a/b"
     to allow matching *)
  let root = Fpath.normalize root in
  let path = Fpath.normalize path in
  (* prepend "./" to relative paths in case one of the paths is "." *)
  let root = make_matchable_relative_path root in
  let path = make_matchable_relative_path path in
  (* add a trailing slash as required by Fpath.rem_prefix (why?) *)
  let path = Fpath.to_dir_path path in
  (* now we can call this function to remove the root prefix from path *)
  match Fpath.rem_prefix root path with
  | None -> if Fpath.equal root path then Some (Fpath.v ".") else None
  | Some rel_path ->
      (* remove the trailing slash if we added one *)
      let rel_path =
        if not had_a_trailing_slash then Fpath.rem_empty_seg rel_path
        else rel_path
      in
      Some rel_path

(*****************************************************************************)
(* Builder entry points *)
(*****************************************************************************)

let of_relative_fpath (fpath : Fpath.t) =
  if Fpath.is_rel fpath then create ("" :: Fpath.segs fpath)
  else invalid_arg ("Ppath.of_relative_fpath: " ^ !!fpath)

(*
   This assumes the input paths are physical paths.

   We use this in tests directly to avoid having to create actual files.
*)
let in_project_unsafe ~(phys_root : Fpath.t) (phys_path : Fpath.t) =
  match remove_prefix phys_root phys_path with
  | None ->
      Error
        (Common.spf
           "cannot make path %S relative to project root %S.\n\
            cwd: %s\n\
            realpath for .: %s\n\
            Sys.argv: %s" !!phys_path !!phys_root (Sys.getcwd ())
           (Rfpath.of_string_exn "." |> Rfpath.show)
           (Sys.argv |> Array.to_list |> String.concat " "))
  | Some rel_path -> Ok (of_relative_fpath rel_path)

let in_project ~(root : Rfpath.t) (path : Rfpath.t) =
  in_project_unsafe
    ~phys_root:(root.rpath |> Rpath.to_fpath)
    (path.rpath |> Rpath.to_fpath)
