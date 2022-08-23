(* This file specifies an intermediate layer that we translate the formula
 * to before matching. Right now, we transform the formula by removing
 * patterns of the form `pattern: $X` when anded with other patterns and
 * saving them as fields of the and. This will make it easier to quickly
 * find the value of that metavariable.
 *)

type selector = {
  mvar : Metavariable.mvar;
  pattern : Pattern.t;
  pid : int;
  pstr : string Rule.wrap;
}
[@@deriving show]

(* These are exactly the same as what are in `Rule.ml`.
    I'm doing it this way to avoid having to make like, 10 different types polymorphic,
   which would make me feel bad and want to kick small puppies.
*)
type taint_source = {
  source_formula : sformula;
  label : string;
  source_requires : AST_generic.expr;
}

and taint_sanitizer = { not_conflicting : bool; sanitizer_formula : sformula }

and taint_propagator = {
  propagate_formula : sformula;
  from : Metavariable.mvar Rule.wrap;
  to_ : Metavariable.mvar Rule.wrap;
}

and taint_sink = { sink_formula : sformula; sink_requires : AST_generic.expr }

and taint_spec = {
  sources : Rule.tok * taint_source list;
  propagators : taint_propagator list;
  sanitizers : taint_sanitizer list;
  sinks : Rule.tok * taint_sink list;
}

and sformula =
  | Leaf of Xpattern.t
  | And of Rule.tok * sformula_and
  | Or of Rule.tok * sformula list
  | Inside of Rule.tok * sformula
  | Taint of Rule.tok * taint_spec
  | Not of Rule.tok * sformula

and sformula_and = {
  selector_opt : selector option;
      (** Invariant: [not (Option.is_some selector_opt) || positives <> []]
          that is, we can only select from a non-empty context. *)
  positives : sformula list;
  negatives : (Rule.tok * sformula) list;
  conditionals : (Rule.tok * Rule.metavar_cond) list;
  focus : (Rule.tok * Metavariable.mvar) list;
}
[@@deriving show]

(*****************************************************************************)
(* Selecting methods *)
(*****************************************************************************)

val selector_equal : selector -> selector -> bool

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

val formula_to_sformula : Rule.formula -> sformula

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

val visit_sformula : (Xpattern.t -> bool -> unit) -> sformula -> unit
