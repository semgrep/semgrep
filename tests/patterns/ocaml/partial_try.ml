let foo x =
  (* ERROR: match *)
  try f x
  with Exn -> 1
