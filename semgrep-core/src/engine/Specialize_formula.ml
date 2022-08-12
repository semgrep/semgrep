module R = Rule
module RM = Range_with_metavars
module G = AST_generic
module MV = Metavariable
module PM = Pattern_match
module RP = Report

type selector = {
  mvar : MV.mvar;
  pattern : AST_generic.any;
  pid : int;
  pstr : string R.wrap;
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
  from : MV.mvar R.wrap;
  to_ : MV.mvar R.wrap;
}

and taint_sink = { sink_formula : sformula; sink_requires : AST_generic.expr }

and taint_spec = {
  sources : taint_source list;
  propagators : taint_propagator list;
  sanitizers : taint_sanitizer list;
  sinks : taint_sink list;
}

and sformula =
  | Leaf of Xpattern.t
  | And of sformula_and
  | Or of sformula list
  | Inside of sformula
  | Taint of taint_spec
  (* There are restrictions on where a Not can appear in a formula. It
   * should always be inside an And to be intersected with "positive" formula.
   *)
  | Not of sformula

and sformula_and = {
  selector_opt : selector option;
  positives : sformula list;
  negatives : sformula list;
  conditionals : R.metavar_cond list;
  focus : MV.mvar list;
}
[@@deriving show]

(*****************************************************************************)
(* Selecting methods *)
(*****************************************************************************)

let selector_equal s1 s2 = s1.mvar = s2.mvar

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let selector_from_formula f =
  match f with
  | Leaf { pat = Sem (pattern, _); pid; pstr } -> (
      match pattern with
      | G.E { e = G.N (G.Id ((mvar, _), _)); _ } when MV.is_metavar_name mvar ->
          Some { mvar; pattern; pid; pstr }
      | _ -> None)
  | _ -> None

let formula_to_sformula formula =
  let rec remove_selectors (selector, acc) formulas =
    match formulas with
    | [] -> (selector, acc)
    | x :: xs ->
        let selector, acc =
          match (selector, selector_from_formula x) with
          | None, None -> (None, x :: acc)
          | Some s, None -> (Some s, x :: acc)
          | None, Some s -> (Some s, acc)
          | Some s1, Some s2 when selector_equal s1 s2 -> (Some s1, acc)
          | Some s1, Some _s2 ->
              (* patterns:
               * ...
               * - pattern: $X
               * - pattern: $Y
               *)
              (* TODO: Should we fail here or just reported as a warning? This
               * is something to catch with the meta-checker. *)
              (Some s1, x :: acc)
        in
        remove_selectors (selector, acc) xs
  in
  let rec formula_to_sformula formula =
    (* Visit formula and convert *)
    match formula with
    | R.P p -> Leaf p
    | R.And { conj_tok = _; conjuncts = fs; conditions = conds; focus } ->
        And (convert_and_formulas fs conds focus)
    | R.Or (_, fs) -> Or (Common.map formula_to_sformula fs)
    | R.Not (_, f) -> Not (formula_to_sformula f)
    | R.Inside (_, f) -> Inside (formula_to_sformula f)
    | R.Taint (_, { sources; propagators; sanitizers; sinks }) ->
        Taint
          {
            sources = Common.map convert_taint_source sources;
            propagators = Common.map convert_taint_propagator propagators;
            sanitizers = Common.map convert_taint_sanitizer sanitizers;
            sinks = Common.map convert_taint_sink sinks;
          }
  and convert_taint_source { R.source_formula; label; source_requires } =
    {
      source_formula = formula_to_sformula source_formula;
      label;
      source_requires;
    }
  and convert_taint_sanitizer { R.sanitizer_formula; not_conflicting } =
    {
      sanitizer_formula = formula_to_sformula sanitizer_formula;
      not_conflicting;
    }
  and convert_taint_sink { R.sink_formula; sink_requires } =
    { sink_formula = formula_to_sformula sink_formula; sink_requires }
  and convert_taint_propagator { R.propagate_formula; from; to_ } =
    { propagate_formula = formula_to_sformula propagate_formula; from; to_ }
  and convert_and_formulas fs cond focus =
    let pos, neg = Rule.split_and fs in
    let pos = Common.map formula_to_sformula pos in
    let neg = Common.map formula_to_sformula neg in
    let sel, pos =
      (* We only want a selector if there is something to select from. *)
      match remove_selectors (None, []) pos with
      | _, [] -> (None, pos)
      | sel, pos -> (sel, pos)
    in
    {
      selector_opt = sel;
      positives = pos;
      negatives = neg;
      conditionals = cond |> Common.map snd;
      focus = focus |> Common.map snd;
    }
  in
  formula_to_sformula formula

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
(* currently used in Match_rules.ml to extract patterns *)
let rec visit_sformula f formula =
  match formula with
  | Leaf p -> f p
  | Not x -> visit_sformula f x
  | Inside sformula -> visit_sformula f sformula
  | Taint { sources; sinks; sanitizers; propagators } ->
      let apply g l =
        Common.map (g (visit_sformula f)) l |> ignore;
        ()
      in
      apply visit_source sources;
      apply visit_propagate propagators;
      apply visit_sink sinks;
      apply visit_sanitizer sanitizers;
      ()
  | Or xs -> xs |> List.iter (visit_sformula f)
  | And fand ->
      fand.positives |> List.iter (visit_sformula f);
      fand.negatives |> List.iter (visit_sformula f)

and visit_source f { source_formula; _ } = f source_formula
and visit_sink f { sink_formula; _ } = f sink_formula
and visit_propagate f { propagate_formula; _ } = f propagate_formula
and visit_sanitizer f { sanitizer_formula; _ } = f sanitizer_formula
