module A = AST_jsonnet
module C = Core_jsonnet

(* TODO: see https://jsonnet.org/ref/spec.html#desugaring *)
let desugar (_e : A.expr) : C.expr = failwith "TODO"
