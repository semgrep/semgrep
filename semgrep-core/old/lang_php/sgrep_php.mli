(* but right now only Expr and Stmt are supported *)
type pattern = Cst_php.any

val parse : string -> pattern

val sgrep :
  ?case_sensitive:bool ->
  hook:(Metavars_php.metavars_binding -> Cst_php.info list -> unit) ->
  Cst_php.any ->
  Common.filename ->
  unit

val sgrep_ast :
  ?case_sensitive:bool ->
  hook:(Metavars_php.metavars_binding -> Cst_php.info list -> unit) ->
  Cst_php.any ->
  Cst_php.program ->
  unit
