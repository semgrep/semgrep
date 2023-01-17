exception IllTyped of string

(* may raise IllTyped *)
val check : Core_jsonnet.expr -> unit
