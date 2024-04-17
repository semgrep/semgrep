let top a b =
  let add_one a = a + 1 [@@trace] in
  add_one a + b

let top a b =
  let add_one a = a + 1 [@@trace_debug] in
  add_one a + b

let top2 a b =
  let%trace sp = "example" in
  a + 1;
  Tracing.add_data_to_span sp [ ("a", `Int 1) ]

let top2 a b =
  let%trace_debug sp = "example" in
  a + 1;
  Tracing.add_data_to_span sp [ ("a", `Int 1) ]
