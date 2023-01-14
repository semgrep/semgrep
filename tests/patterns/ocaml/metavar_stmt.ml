let foo = 
  let v = 1 in
  (* ERROR: *)
  if v > 2 then 1 else 2
