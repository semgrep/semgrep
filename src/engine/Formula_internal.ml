module MV = Metavariable
open Ppx_hash_lib.Std.Hash.Builtin

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tok = Tok.t_always_equal [@@deriving show, eq, hash]

type formula =
  | P of Xpattern.t (* a leaf pattern *)
  | And of tok * formula list
  | Or of tok * formula list
  | Not of tok * formula
  | Inside of tok * formula
  | Anywhere of tok * formula
  | Filter of tok * filter * formula
  | Focus of tok * MV.mvar list * formula

and filter =
  | FilterEval of AST_generic.expr (* see Eval_generic.ml *)
  | FilterRegexp of
      MV.mvar * Xpattern.regexp_string * bool (* constant-propagation *)
  | FilterType of
      MV.mvar
      * Xlang.t option
      (* when the type expression is in different lang *)
      * string list (* raw input string saved for regenerating rule yaml *)
      * AST_generic.type_ list
    (* LATER: could parse lazily, like the patterns *)
  | FilterAnalysis of MV.mvar * metavar_analysis_kind
  | FilterNestedFormula of MV.mvar * Xlang.t option * formula

and metavar_analysis_kind = Rule.metavar_analysis_kind
[@@deriving show, eq, hash]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let rec of_rule_formula ({ f; conditions; focus } : Rule.formula) : formula =
  let inner_formula = of_rule_formula_kind f in
  let filtered_formula =
    List.fold_left
      (fun acc (tk, cond) ->
        let filter = filter_of_rule_cond cond in
        Filter (tk, filter, acc))
      inner_formula conditions
  in
  let focused_formula =
    List.fold_left
      (fun acc (tk, mvars) -> Focus (tk, mvars, acc))
      filtered_formula focus
  in
  focused_formula

and of_rule_formula_kind (kind : Rule.formula_kind) : formula =
  match kind with
  | Rule.P pat -> P pat
  | And (tk, fs) -> And (tk, List_.map of_rule_formula fs)
  | Or (tk, fs) -> Or (tk, List_.map of_rule_formula fs)
  | Not (tk, f) -> Not (tk, of_rule_formula f)
  | Inside (tk, f) -> Inside (tk, of_rule_formula f)
  | Anywhere (tk, f) -> Anywhere (tk, of_rule_formula f)

and filter_of_rule_cond = function
  | Rule.CondEval e -> FilterEval e
  | CondRegexp (mvar, s, b) -> FilterRegexp (mvar, s, b)
  | CondType (mv, xlang, ty_strs, tys) -> FilterType (mv, xlang, ty_strs, tys)
  | CondAnalysis (mvar, analysis_kind) -> FilterAnalysis (mvar, analysis_kind)
  | CondNestedFormula (mvar, xlang, f) ->
      FilterNestedFormula (mvar, xlang, of_rule_formula f)

let split_pos_neg f =
  match f with
  | P _
  | And _
  | Or _
  | Anywhere _
  | Inside _ ->
      Either.Left f
  (* TODO: This is actually not correct.
     A `filter` or `focus` could be wrapping a `Not`,
     which means that this is overall actually a negative
     pattern.
     This can be fixed by making evaluate_formula itself
     bubble up the information of patterns being positive or
     negative, which is a later change to make.
  *)
  | Filter _
  | Focus _ ->
      Left f
  | Not (tk, f) -> Right (tk, f)

(* OK, this is only a little disgusting, but...
   Evaluation order means that we will only visit children after parents.
   So we keep a reference cell around, and set it to true whenever we descend
   under an inside.
   That way, pattern leaves underneath an Inside/Anywhere will properly be
   paired with a true boolean.
*)
let visit_formula func formula =
  let bref = ref false in
  let rec visit_formula func formula =
    match formula with
    | P p -> func p ~inside:!bref
    | Anywhere (_, formula)
    | Inside (_, formula)
    | Filter (_, _, formula)
    | Focus (_, _, formula) ->
        Common.save_excursion bref true (fun () -> visit_formula func formula)
    | Not (_, x) -> visit_formula func x
    | Or (_, xs)
    | And (_, xs) ->
        xs |> List.iter (visit_formula func)
  in
  visit_formula func formula
