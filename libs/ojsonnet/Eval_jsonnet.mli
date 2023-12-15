(* Rely on Conf_ojsonnet flags *)

val eval_program : Core_jsonnet.program -> Value_jsonnet.t
val manifest_value : Value_jsonnet.t -> JSON.t
