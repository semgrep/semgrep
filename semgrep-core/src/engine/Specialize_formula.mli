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
  | Leaf of Rule.leaf
  | And of (selector option * sformula list)
      (** Invariant: [And (sel_opt, fs)] satisfies
     * [not (Option.is_some sel_opt) || fs <> []], that is,
     * we can only select from a non-empty context.
     *)
  | Or of sformula list
  | Not of sformula
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
