let handle f = match f () with
  (* ERROR: match *)
  | effect Read, k -> continue k 42
  | effect Write, k -> continue k 0
  | x -> x
