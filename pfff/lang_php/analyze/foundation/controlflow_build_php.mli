(*s: controlflow_build_php.mli *)

(*s: controlflow builders signatures *)
val control_flow_graph_of_stmts: Cst_php.dname list -> Cst_php.stmt list -> Controlflow_php.flow
(* alias *)
val cfg_of_stmts: Cst_php.dname list -> Cst_php.stmt list -> Controlflow_php.flow

val cfg_of_func:   Cst_php.func_def -> Controlflow_php.flow
(*e: controlflow builders signatures *)

(*s: controlflow checkers signatures *)
val deadcode_detection : Controlflow_php.flow -> unit
(*e: controlflow checkers signatures *)

(*s: type Controlflow_build_php.error *)
type error = error_kind * Cst_php.info
 and error_kind = 
  | DeadCode of Controlflow_php.node_kind
  | NoEnclosingLoop
  | ColonSyntax
  | DynamicBreak
(*e: type Controlflow_build_php.error *)

val string_of_error: error -> string
val string_of_error_kind: error_kind -> string
(*s: error exception and report_error signature *)
exception Error of error

val report_error : error -> unit
(*e: error exception and report_error signature *)
(*x: controlflow_build_php.mli *)
(*e: controlflow_build_php.mli *)
