let test () =
  (*ERROR: match *)
  let _ = foo 1 2 3 1 2 in
  let _ = foo 1 2 3 4 5 in
  ()
