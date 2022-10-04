let foo () =
  (* ERROR: match *)
  let expr = 1 in
  (* ERROR: match *)
  let foo = 2 + 3 in
  foo + expr
