module C = Core_jsonnet

exception IllTyped of string

(* TODO https://jsonnet.org/ref/spec.html#static_checking *)

let check (_e : C.expr) : unit = failwith "TODO"
