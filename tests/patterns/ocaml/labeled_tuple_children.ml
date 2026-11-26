let values x =
  (* ERROR: match *)
  (~data:(sink x), ~other:0)
let typed x =
  (* ERROR: match *)
  (~(x : int), sink x)
let negative x = (~data:(safe x), 0)
