let foo = function
  | X1 -> 2
  | X2 -> 3

let bar = function
  | X1 -> 2
  (* ERROR: match *)
  | _ -> 3
