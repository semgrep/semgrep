let foo x =
  (* ERROR: match *)
  match x with
  | Foo -> 1
  | Bar -> 2


let bar y =
  (* ERROR: match *)
  match y with
  | Foo -> 1
  | Bar -> 2
