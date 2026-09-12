(*
   Unit tests for the Protect_ident module.
*)

open Printf
open Tree_sitter_gen

let test_translate () =
  let reserved_dst = ["foo"; "bar_"] in
  let translations = [
    "thing", None;
    "foo", Some "foo_";
    "bar", None;
    "bar_", Some "bar__";
  ] in
  let registry = Protect_ident.create ~reserved_dst () in
  List.iter (fun (src, expected_dst) ->
    let dst = Protect_ident.translate registry src in
    printf "%s -> %s\n%!"
      src
      (match dst with
       | None -> "None"
       | Some s -> sprintf "Some %S" s);
    Alcotest.(check bool) "equal" true (expected_dst = dst)
  ) translations

let test_reserve () =
  let reserved_dst = ["special"] in
  let preferred_translations = [
    ("foo", "foo"), "foo";
    ("_foo", "foo"), "foo_";
    ("__foo", "foo"), "foo2";
    ("_foo", "foo"), "foo_";
    ("any", "special"), "special2";
  ] in
  let registry = Protect_ident.create ~reserved_dst () in
  List.iter (fun ((src, preferred_dst), expected_dst) ->
    let dst = Protect_ident.add_translation registry src ~preferred_dst in
    printf "%s -> %s\n%!" src dst;
    Alcotest.(check string) "match" expected_dst dst
  ) preferred_translations

let test = "Protect_ident", [
  "translate", `Quick, test_translate;
  "reserve", `Quick, test_reserve;
]
