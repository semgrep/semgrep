module G = AST_generic

let foo () =
  (* ERROR: match *)
  G.Call (1,2);
  E.Call (1,2)
