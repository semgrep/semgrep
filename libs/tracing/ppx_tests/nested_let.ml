let top a b =
  let add_one a = a + 1 [@@trace] in
  add_one a + b

let top2 a b =
  let%trace sp = "example" in
  a + 1;
  Tracing.add_data_to_span sp [ ("a", `Int 1) ]
