
val gen_layer: 
  root:Common.dirname -> output:Common.filename -> 
  Error_php.error list -> 
  unit

(* internal, reused by layer_check_module.ml *)
val properties: (string * string) list
val info_of_error_and_kind: Error_php.error -> Cst_php.info * string
