(*s: pfff/lang_GENERIC/analyze/Dataflow_tainting.mli *)

(*s: type [[Dataflow_tainting.mapping]] *)
(* map for each node/var whether a variable is "tainted" *)
type mapping = unit Dataflow.mapping
(*e: type [[Dataflow_tainting.mapping]] *)

(* Tracks tainted functions. *)
type fun_env = (Dataflow.var, unit) Hashtbl.t

(*s: type [[Dataflow_tainting.config]] *)
(* this can use semgrep patterns under the hood *)
type config = {
  is_source: IL.instr -> bool;
  is_source_exp: IL.exp -> bool;
  is_sink: IL.instr -> bool;
  is_sanitizer: IL.instr -> bool;

  found_tainted_sink: IL.instr -> unit Dataflow.env -> unit;
}
(*e: type [[Dataflow_tainting.config]] *)

(*s: signature [[Dataflow_tainting.fixpoint]] *)
(* main entry point *)
val fixpoint : config -> fun_env -> IL.name option -> IL.cfg -> mapping
(*e: signature [[Dataflow_tainting.fixpoint]] *)
(*e: pfff/lang_GENERIC/analyze/Dataflow_tainting.mli *)
