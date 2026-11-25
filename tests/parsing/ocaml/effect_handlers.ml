let handle f =
  match f () with
  | effect Read, k -> continue k 42
  | effect (Write value), k when value > 0 -> continue k ()
  | value -> value
