let foo a b = a + b [@@trace]
and bar a b = a + b [@@other]

let top a b =
  let add_one a = a + 1 [@@trace] in
  add_one a + b

let just_add a b = a + b
let label a b = a + b [@@trace "example function name"]
