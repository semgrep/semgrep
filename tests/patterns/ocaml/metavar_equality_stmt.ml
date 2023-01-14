let foo = 
  (* ERROR: *)
  if x > 2 then 
    foo
  else 
    foo

let foo2 = 
  if x > 2 then
    bar
  else 
    foo
