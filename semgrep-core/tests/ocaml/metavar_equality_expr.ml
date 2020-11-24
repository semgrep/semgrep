let foo = 
  let a = 1 in
  let b = 2 in
  (* ERROR: *)
  if a+b = a+b then 1 else 2
