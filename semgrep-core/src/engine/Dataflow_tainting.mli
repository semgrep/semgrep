type var = Dataflow_core.var
(** A string of the form "<source name>:<sid>". *)

type trace = AST_generic.tok list [@@deriving show]
(** A trace of tokens showing where the taint passed through. *)

(** A match that spans multiple functions (aka "deep").
  * E.g. Call('foo(a)', PM('sink(x)')) is an indirect match for 'sink(x)'
  * through the function call 'foo(a)'. *)
type 'a deep_match =
  | PM of Pattern_match.t * 'a  (** A direct match.  *)
  | Call of AST_generic.expr * trace * 'a deep_match
      (** An indirect match through a function call. *)
[@@deriving show]

type source = Rule.taint_source deep_match [@@deriving show]
type sink = Rule.taint_sink deep_match [@@deriving show]
type arg_pos = int [@@deriving show]

type source_to_sink = {
  source : source;
  trace : trace;
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

(** Function-level finding (not necessarily a Semgrep finding). These may
  * depend on taint variables so they must be interpreted on a specific
  * context. *)
type finding =
  | SrcToSink of source_to_sink
  | SrcToReturn of source * trace * AST_generic.tok
  | ArgToSink of arg_pos * trace * sink
  | ArgToReturn of arg_pos * trace * AST_generic.tok
[@@deriving show]

type taint_orig =
  | Src of source  (** An actual taint source (`pattern-sources:` match). *)
  | Arg of arg_pos
      (** A taint variable (potential taint coming through an argument). *)
[@@deriving show]

type taint = { orig : taint_orig; rev_trace : trace } [@@deriving show]

module Taint : Set.S with type elt = taint
(** A set of taint sources. *)

type config = {
  filepath : Common.filename;  (** File under analysis, for Deep Semgrep. *)
  rule_id : string;  (** Taint rule id, for Deep Semgrep. *)
  is_source :
    AST_generic.any -> (Pattern_match.t * float * Rule.taint_source) list;
      (** Test whether 'any' is a taint source, this corresponds to
      * 'pattern-sources:' in taint-mode. *)
  is_sink : AST_generic.any -> (Pattern_match.t * Rule.taint_sink) list;
      (** Test whether 'any' is a sink, this corresponds to 'pattern-sinks:'
      * in taint-mode. *)
  is_sanitizer : AST_generic.any -> Pattern_match.t list;
      (** Test whether 'any' is a sanitizer, this corresponds to
      * 'pattern-sanitizers:' in taint-mode. *)
  unify_mvars : bool;  (** Unify metavariables in sources and sinks? *)
  handle_findings :
    var option (** function name ('None' if anonymous) *) ->
    finding list ->
    Taint.t Dataflow_core.env ->
    unit;
      (** Callback to report findings. *)
}
(** Taint rule instantiated for a given file.
  *
  * For a source to taint a sink, the bindings of both source and sink must be
  * unifiable. See 'unify_meta_envs'. *)

type mapping = Taint.t Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

type fun_env = (var, Taint.t) Hashtbl.t
(** Set of functions known to act as taint sources (their output is
  * tainted). This is used for a HACK to do some poor-man's intrafile
  * interprocedural taint tracking. TO BE DEPRECATED. *)

val pm_of_dm : 'a deep_match -> Pattern_match.t * 'a

val hook_function_taint_signature :
  (config -> AST_generic.expr -> finding list option) option ref
(** Deep Semgrep *)

val fixpoint :
  ?in_env:Taint.t Dataflow_core.VarMap.t ->
  ?name:var ->
  ?fun_env:fun_env (** Poor-man's interprocedural HACK (TO BE DEPRECATED) *) ->
  config ->
  IL.cfg ->
  mapping
(** Main entry point, [fixpoint config cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [config.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * @param fun_env is a set of tainted functions in the same file, it is a HACK
  *    to provide some poor-man's interprocedural capabilities. If the function
  *    being analyzed has a name, and taint reaches a `return` statement, the
  *    function name will be added to this environment by side-effect.
  * *)
