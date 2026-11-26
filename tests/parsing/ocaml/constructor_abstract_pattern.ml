let f e =
  match e with
  | C (type a) (x : a) -> x
  | effect E (type a) (x : a), k -> continue k x
