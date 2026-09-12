(*
   Unit tests for the Fresh module.
*)

open Printf
open Tree_sitter_gen

let test_simple () =
  let scope = Fresh.create_scope () in
  let add name =
    let res = Fresh.create_name scope name in
    printf "%s -> %s\n%!" name res;
    res
  in
  assert (add "x" = "x");
  assert (add "y" = "y");
  assert (add "x" = "x_");
  assert (add "x" = "x2")

let test_prefix_prefix () =
  let scope = Fresh.create_scope () in
  let add name =
    let res = Fresh.create_name scope name in
    printf "%s -> %s\n%!" name res;
    res
  in
  assert (add "x" = "x");
  assert (add "x_" = "x_");
  assert (add "x" = "x2");
  assert (add "x_" = "x__");
  assert (add "x_" = "x_2")

let test = "Fresh", [
  "simple", `Quick, test_simple;
  "prefix of prefix", `Quick, test_prefix_prefix;
]
