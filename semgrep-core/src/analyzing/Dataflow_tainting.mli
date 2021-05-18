(*s: pfff/lang_GENERIC/analyze/Dataflow_tainting.mli *)

(*s: type [[Dataflow_tainting.mapping]] *)
type mapping = unit Dataflow.mapping
(** Map for each node/var whether a variable is "tainted" *)

(*e: type [[Dataflow_tainting.mapping]] *)

type fun_env = (Dataflow.var, unit) Hashtbl.t
(** Set of "tainted" functions in the overall program.
  * Note that here [Dataflow.var] is a string of the form "<source name>:<sid>". *)

(*s: type [[Dataflow_tainting.config]] *)
type config = {
  is_source : IL.instr -> bool;
  is_source_exp : IL.exp -> bool;
  is_sink : IL.instr -> bool;
  is_sanitizer : IL.instr -> bool;
  found_tainted_sink : IL.instr -> unit Dataflow.env -> unit;
}
(** This can use semgrep patterns under the hood. Note that a source can be an
  * instruction but also an expression. *)

(*e: type [[Dataflow_tainting.config]] *)

(*s: signature [[Dataflow_tainting.fixpoint]] *)

val fixpoint : config -> fun_env -> IL.name option -> IL.cfg -> mapping
(** Main entry point, [fixpoint config fun_env opt_name cfg] returns a mapping
  * (effectively a set) containing all the tainted variables in [cfg]. Besides,
  * if it finds an instruction that consumes tainted data, then it will invoke
  * [config.found_tainted_sink] which can perform any side-effectful action.
  *
  * Parameter [fun_env] is a set of tainted functions in the overall program;
  * it provides basic interprocedural capabilities.
  *
  * Parameter [opt_name] is the name of the function being analyzed, if it has
  * a name. When [Some name] is given, and there is a tainted return statement in
  * [cfg], the function [name] itself will be added to [fun_env] by side-effect.
*)

(*e: signature [[Dataflow_tainting.fixpoint]] *)
(*e: pfff/lang_GENERIC/analyze/Dataflow_tainting.mli *)
