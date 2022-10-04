module G = AST_generic

let foo () =
  (* ERROR: match *)
  G.fb [];
  E.fb [];
  (* ERROR: match *)
  AST_generic.fb [];

  ()


    
