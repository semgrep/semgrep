(*
   Abstract type for a file path within a git project
*)

type t = { string : string; components : string list }

let of_string string =
  let components =
    match String.split_on_char '/' string with
    | [ "" ] -> (* should be an error? *) [ "." ]
    | [] -> assert false
    | x -> x
  in
  { string; components }

let to_string x = x.string

let check_component str =
  if String.contains str '/' then
    invalid_arg
      ("Git_path.create: path component may not contain a slash: " ^ str)

let unsafe_create components =
  { string = String.concat "/" components; components }

let create components =
  List.iter check_component components;
  unsafe_create components

let append_component xs x =
  let rec loop xs =
    match xs with
    | [] -> [ x ]
    | [ "" ] -> (* ignore trailing slash that's not a leading slash *) [ x ]
    | x :: xs -> x :: loop xs
  in
  match xs with
  | "" :: xs -> "" :: loop xs
  | xs -> loop xs

let append path comp =
  check_component comp;
  let components = append_component path.components comp in
  unsafe_create components

module Ops = struct
  let ( / ) = append
end

let components x = x.components

let is_absolute x =
  match x.components with
  | "" :: _ -> true
  | __else__ -> false

let is_relative x = not (is_absolute x)

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
  match x.components with
  | "" :: xs -> (
      match normalize xs with
      | ".." :: _ -> Error ("invalid git path: " ^ x.string)
      | [] -> Ok (create [ ""; "" ])
      | components -> Ok (create ("" :: components)))
  | xs ->
      let components =
        match normalize xs with
        | [] -> [ "." ]
        | xs -> xs
      in
      Ok (create components)

let of_fpath path = Fpath.segs path |> create
let root = of_string "/"

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
      test_append "/a/" "c" "/a/c")
