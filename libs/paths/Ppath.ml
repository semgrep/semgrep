open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Abstract type for a file path within a project

   The name of the module imitates Fpath.ml, but use Ppath.ml for
   Project path (instead of File path).
*)

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
   * Invariant: the first element of the list should always be "",
   * because all ppaths are absolute paths.
   *)
  segments : string list;
  (* String.concat "/" segments
   * TODO: get rid of it? just compute it dynamically?
   *)
  string : string;
}

(* old: was of_string_for_tests "/" *)
let root = { string = "/"; segments = [ ""; "" ] }

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* Useful to debug, to use in error messages, or when passing the ppath
 * to a regexp matcher (e.g., Glob.Match.run()).
 * However, you should prefer to_fpath() most of the time, and then
 * Fpath.to_string() if needed.
 *)
let to_string x = x.string

(* TODO: make a rel_segments function so the caller does not have to do
   let rel_segments =
      match Ppath.segments full_git_path with
      | "" :: xs -> xs
      | __else__ -> assert false
    in
*)

let segments x = x.segments

(*****************************************************************************)
(* Builder helpers (not exposed in Ppath.mli) *)
(*****************************************************************************)

let check_segment str =
  if String.contains str '/' then
    invalid_arg ("Ppath.create: path segment may not contain a slash: " ^ str)

let unsafe_create segments = { string = String.concat "/" segments; segments }

(* Note that this does not ensure the segments represent an absolute path
 * and this does not normalize either those segments.
 * Use from_segments( or in_project() instead.
 *)
let create segments =
  List.iter check_segment segments;
  unsafe_create segments

(*****************************************************************************)
(* Append *)
(*****************************************************************************)

let append_segment xs x =
  let rec loop xs =
    match xs with
    | [] -> [ x ]
    | [ "" ] -> (* ignore trailing slash that's not a leading slash *) [ x ]
    | x :: xs -> x :: loop xs
  in
  match xs with
  | "" :: xs -> "" :: loop xs
  (* TODO: this case should not happen anymore now *)
  | xs -> loop xs

(* use same terminology than in Fpath *)
let add_seg path seg =
  check_segment seg;
  let segments = append_segment path.segments seg in
  unsafe_create segments

(* saving you 3 neurons *)
let add_segs path segs = List.fold_left add_seg path segs

let append_fpath path fpath =
  match Fpath.segs fpath with
  | "" :: _ ->
      invalid_arg
        ("Ppath.append_fpath: not a relative path: " ^ Fpath.to_string fpath)
  | segs -> add_segs path segs

module Operators = struct
  let ( / ) = add_seg
end

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)
let to_fpath ~root path =
  match path.segments with
  | "" :: segments ->
      List.fold_left Fpath.add_seg root segments
      |> (* remove leading "./" typically occuring when the project root
            is "." *)
      Fpath.normalize
  | _ -> assert false

(*****************************************************************************)
(* Project Builder *)
(*****************************************************************************)

(* TODO: delete *)
(* A ppath should always be absolute! *)
let is_absolute x =
  match x.segments with
  | "" :: _ -> true
  | __else__ -> false

(* TODO: delete *)
let is_relative x = not (is_absolute x)

(* TODO: delete *)
let make_absolute x =
  if is_relative x then { string = "/" ^ x.string; segments = "" :: x.segments }
  else x

let rec normalize (xs : string list) : string list =
  match xs with
  | ".." :: xs -> ".." :: normalize xs
  | [ "" ] as xs (* preserve trailing slash *) -> xs
  | ("." | "") :: xs -> normalize xs
  | _ :: ".." :: xs -> normalize xs
  | x :: xs as orig ->
      let res = normalize xs in
      (* If nothing changes via normalization, return the original list *)
      if Stdlib.( == ) res xs then orig
      else (* Something changed, make another pass *)
        normalize (x :: res)
  | [] -> []

let normalize_ppath x =
  match x.segments with
  | "" :: xs -> (
      match normalize xs with
      | ".." :: _ -> Error ("invalid git path: " ^ x.string)
      | [] -> Ok (create [ ""; "" ])
      | segments -> Ok (create ("" :: segments)))
  (* TODO: delete, this should not happen anymore *)
  | xs ->
      let segments =
        match normalize xs with
        | [] -> [ "." ]
        | xs -> xs
      in
      Ok (create segments)

let of_fpath path =
  Fpath.segs path |> create |> make_absolute |> normalize_ppath

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

let in_project ~root path =
  match remove_prefix root path with
  | None ->
      Error
        (Common.spf "cannot make path %S relative to project root %S" !!path
           !!root)
  | Some path -> path |> of_fpath

let from_segments segs =
  match segs with
  | "" :: _ ->
      let ppath = create segs in
      normalize_ppath ppath
  | _ -> Error "segments do not represent an absolute path"

(*****************************************************************************)
(* Tests helpers *)
(*****************************************************************************)

let of_string_for_tests string =
  let segments =
    match String.split_on_char '/' string with
    | [ "" ] -> (* should be an error? *) [ "." ]
    | [] -> assert false
    | x -> x
  in
  { string; segments }

(*****************************************************************************)
(* Inline tests *)
(*****************************************************************************)

let () =
  Alcotest_ext.test "Ppath" (fun () ->
      let test_str f input expected_output =
        Alcotest.(check string) "equal" expected_output (f input)
      in
      let rewrite str = to_string (of_string_for_tests str) in
      test_str rewrite "/" "/";
      test_str rewrite "//" "//";
      test_str rewrite "" "";
      test_str rewrite "a/" "a/";

      let norm str =
        match of_string_for_tests str |> normalize_ppath with
        | Ok x -> to_string x
        | Error s -> failwith s
      in
      let norm_err str =
        match of_string_for_tests str |> normalize_ppath with
        | Ok _ -> false
        | Error _ -> true
      in
      test_str norm "a" "a";
      test_str norm "a/b" "a/b";
      test_str norm "ab/cd" "ab/cd";
      test_str norm "/" "/";
      test_str norm "/a" "/a";
      test_str norm "/a/b" "/a/b";
      test_str norm "" ".";
      test_str norm "." ".";
      test_str norm "." ".";
      test_str norm ".." "..";
      assert (norm_err "/..");
      test_str norm "a/../b" "b";
      test_str norm "a/.." ".";
      test_str norm "a/../.." "..";
      assert (norm_err "/a/../..");
      test_str norm "a/b/../c/d/e/../.." "a/c";
      test_str norm "/a/b/../c/d/e/../.." "/a/c";
      test_str norm "a/" "a/";
      test_str norm "/a/" "/a/";
      test_str norm "/a/b/" "/a/b/";

      let test_add_seg a b ab =
        Alcotest.(check string)
          "equal" ab
          (add_seg (of_string_for_tests a) b |> to_string)
      in
      test_add_seg "/" "a" "/a";
      test_add_seg "/a" "b" "/a/b";
      test_add_seg "/a/" "c" "/a/c";

      let test_in_project_ok root path expected =
        match in_project ~root:(Fpath.v root) (Fpath.v path) with
        | Ok res -> Alcotest.(check string) "equal" expected (to_string res)
        | Error msg -> Alcotest.fail msg
      in
      let test_in_project_fail root path =
        match in_project ~root:(Fpath.v root) (Fpath.v path) with
        | Ok res -> Alcotest.fail (to_string res)
        | Error _ -> ()
      in
      test_in_project_ok "/a" "/a/b" "/b";
      test_in_project_ok "/a" "/a" "/";
      test_in_project_ok "/a" "/a/b/c" "/b/c";
      test_in_project_ok "/a" "/a/b/c/d" "/b/c/d";
      test_in_project_ok "/a/b" "/a/b/c/d" "/c/d";
      test_in_project_ok "/a/" "/a/b" "/b";
      test_in_project_ok "/a" "/a/b/" "/b/";
      test_in_project_ok "/a/b" "/a/b/c/.." "/";
      test_in_project_ok "/a/b" "/a/b/c/../" "/";
      test_in_project_ok "/a/b" "/a/b/./c/." "/c/";
      test_in_project_ok "a/b" "a/b/c" "/c";
      test_in_project_ok "." "a/b" "/a/b";
      test_in_project_ok "a" "./a/b" "/b";
      test_in_project_ok "." "." "/";
      test_in_project_ok "a/b" "a/b" "/";
      test_in_project_ok "/a/b" "/a/b" "/";
      test_in_project_fail "/a/b" "/a";
      test_in_project_fail "/a/b" "/b";
      test_in_project_fail "/a/b" "a")
