(*
   Abstract type for a file path within a git project
*)

open Printf
open File.Operators

type t = { string : string; segments : string list }

let of_string string =
  let segments =
    match String.split_on_char '/' string with
    | [ "" ] -> (* should be an error? *) [ "." ]
    | [] -> assert false
    | x -> x
  in
  { string; segments }

let root = of_string "/"
let to_string x = x.string

let check_segment str =
  if String.contains str '/' then
    invalid_arg ("Git_path.create: path segment may not contain a slash: " ^ str)

let unsafe_create segments = { string = String.concat "/" segments; segments }

let create segments =
  List.iter check_segment segments;
  unsafe_create segments

let append_segment xs x =
  let rec loop xs =
    match xs with
    | [] -> [ x ]
    | [ "" ] -> (* ignore trailing slash that's not a leading slash *) [ x ]
    | x :: xs -> x :: loop xs
  in
  match xs with
  | "" :: xs -> "" :: loop xs
  | xs -> loop xs

let append path seg =
  check_segment seg;
  let segments = append_segment path.segments seg in
  unsafe_create segments

module Ops = struct
  let ( / ) = append
end

let segments x = x.segments

let is_absolute x =
  match x.segments with
  | "" :: _ -> true
  | __else__ -> false

let is_relative x = not (is_absolute x)

let make_absolute x =
  if is_relative x then { string = "/" ^ x.string; segments = "" :: x.segments }
  else x

let normalize x =
  let rec normalize xs =
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
  in
  match x.segments with
  | "" :: xs -> (
      match normalize xs with
      | ".." :: _ -> Error ("invalid git path: " ^ x.string)
      | [] -> Ok (create [ ""; "" ])
      | segments -> Ok (create ("" :: segments)))
  | xs ->
      let segments =
        match normalize xs with
        | [] -> [ "." ]
        | xs -> xs
      in
      Ok (create segments)

let of_fpath path = Fpath.segs path |> create

let to_fpath ~root path =
  let rec append fpath segments =
    match segments with
    | [] -> fpath
    | seg :: segments -> append (Fpath.add_seg fpath seg) segments
  in
  let rel_segments =
    match path.segments with
    | "" :: segments
    | segments ->
        segments
  in
  append root rel_segments

let in_project ~root path =
  (* This doesn't work if path doesn't have a trailing slash.
     If we run into more problems and what's going on here is too obscure,
     maybe it's best to write our own version of rem_prefix. *)
  let has_trailing_slash = Fpath.is_dir_path path in
  match Fpath.rem_prefix root (Fpath.to_dir_path path) with
  | None ->
      Error
        (sprintf "cannot make path %S relative to project root %S" !!path !!root)
  | Some path ->
      let path =
        if has_trailing_slash then path else Fpath.rem_empty_seg path
      in
      path |> of_fpath |> make_absolute |> normalize

let () =
  Testutil.test "Git_path" (fun () ->
      let test_str f input expected_output =
        Alcotest.(check string) "equal" expected_output (f input)
      in
      let rewrite str = to_string (of_string str) in
      test_str rewrite "/" "/";
      test_str rewrite "//" "//";
      test_str rewrite "" "";
      test_str rewrite "a/" "a/";

      let norm str =
        match of_string str |> normalize with
        | Ok x -> to_string x
        | Error s -> failwith s
      in
      let norm_err str =
        match of_string str |> normalize with
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

      let test_append a b ab =
        Alcotest.(check string) "equal" ab (append (of_string a) b |> to_string)
      in
      test_append "/" "a" "/a";
      test_append "/a" "b" "/a/b";
      test_append "/a/" "c" "/a/c";

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
      test_in_project_fail "/a/b" "/a";
      test_in_project_fail "/a/b" "/b";
      test_in_project_fail "/a/b" "a";
      test_in_project_fail "/a/b" "b")
