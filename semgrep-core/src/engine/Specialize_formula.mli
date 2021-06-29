(* This file specifies an intermediate layer that we translate the formula to *)
(* before matching. Right now, we transform the formula by removing patterns  *)
(* of the form `pattern: $X` when anded with other patterns and saving them   *)
(* as fields of the and. This will make it easier to quickly find the value   *)
(* of that metavariable. *)

type selector = {
  mvar : Metavariable.mvar;
  pid : int;
  pstr : string;
  (* lazy_matches necessary in case `$X` is the only pattern *)
  lazy_matches : Report.times Report.match_result Lazy.t;
}

type sformula =
  | Leaf of Rule.leaf
  | And of (selector option * sformula list)
  | Or of sformula list
  | Not of sformula

(*****************************************************************************)
(* Selecting methods *)
(*****************************************************************************)

val fake_rule_id : int * string -> Pattern_match.rule_id

val match_selector :
  ?err:string -> selector option -> Range_with_metavars.ranges

val select_from_ranges :
  string ->
  selector option ->
  Range_with_metavars.ranges ->
  Range_with_metavars.ranges

val selector_equal : selector -> selector -> bool

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

val formula_to_sformula :
  ((AST_generic.any * int * string) list -> Report.times Report.match_result) ->
  Rule.formula ->
  sformula

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

val visit_sformula : (Rule.xpattern -> unit) -> sformula -> unit
