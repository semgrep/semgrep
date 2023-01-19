let foo = 
  let x = 1 in
  (* ERROR: *)
  if x > 2 then foo else bar
