(* test file for semgrep-core -lsp -e '...' test_lsp.ml  after dune build *)
module G = AST_generic
open AST_generic

let foo e =
  let res0 = AST_generic.Call (Int (None, fake ""), fb []) in
  let res1 = G.Call (Int (None, fake ""), fb []) in
  let res2 = Call (Int (None, fake ""), fb []) in
  match e.e with
  | AST_generic.Call (x, (_, [], _)) -> 1
  | G.Call (x, (_, [ _ ], _)) -> 1
  | Call (x, y) -> 1
  | _ -> 2

let bar () =
  AST_generic.fake_bracket [] |> ignore;
  G.fake_bracket [] |> ignore;
  fake_bracket [] |> ignore
