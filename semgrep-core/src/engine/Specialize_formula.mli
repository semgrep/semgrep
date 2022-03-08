(* This file specifies an intermediate layer that we translate the formula to *)
(* before matching. Right now, we transform the formula by removing patterns  *)
(* of the form `pattern: $X` when anded with other patterns and saving them   *)
(* as fields of the and. This will make it easier to quickly find the value   *)
(* of that metavariable. *)

type selector = {
  mvar : Metavariable.mvar;
  pattern : AST_generic.any;
  pid : int;
  pstr : string Rule.wrap;
}
[@@deriving show]

type sformula =
  | Leaf of Rule.xpattern * Rule.inside option
  | And of sformula_and
  | Or of sformula list
  | Not of sformula

and sformula_and = {
  selector_opt : selector option;
      (** Invariant: [not (Option.is_some selector_opt) || positives <> []]
          that is, we can only select from a non-empty context. *)
  positives : sformula list;
  negatives : sformula list;
  conditionals : Rule.metavar_cond list;
  focus : Metavariable.mvar list;
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

val visit_sformula :
  (Rule.xpattern -> Rule.inside option -> unit) -> sformula -> unit
