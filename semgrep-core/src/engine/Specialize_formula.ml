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
   I'm doing it this way to avoid having to make like, 10 different types polymorphic.
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
  (* There are restrictions on where a Not can appear in a formula. It
   * should always be inside an And to be intersected with "positive" formula.
   *)
  | Not of Rule.tok * sformula

and sformula_and = {
  selector_opt : selector option;
  positives : sformula list;
  negatives : (Rule.tok * sformula) list;
  conditionals : (Rule.tok * R.metavar_cond) list;
  focus : (Rule.tok * MV.mvar) list;
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
    | R.And { conj_tok; conjuncts = fs; conditions = conds; focus } ->
        And (conj_tok, convert_and_formulas fs conds focus)
    | R.Or (tok, fs) -> Or (tok, Common.map formula_to_sformula fs)
    | R.Not (tok, f) -> Not (tok, formula_to_sformula f)
    | R.Inside (tok, f) -> Inside (tok, formula_to_sformula f)
    | R.Taint
        ( tok,
          {
            sources = source_tok, sources;
            propagators;
            sanitizers;
            sinks = sink_tok, sinks;
          } ) ->
        Taint
          ( tok,
            {
              sources = (source_tok, Common.map convert_taint_source sources);
              propagators = Common.map convert_taint_propagator propagators;
              sanitizers = Common.map convert_taint_sanitizer sanitizers;
              sinks = (sink_tok, Common.map convert_taint_sink sinks);
            } )
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
  and convert_and_formulas fs conditionals focus =
    let pos, neg = Rule.split_and fs in
    let pos = Common.map formula_to_sformula pos in
    let neg = Common.map (fun (t, f) -> (t, formula_to_sformula f)) neg in
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
      conditionals;
      focus;
    }
  in
  formula_to_sformula formula

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
(* currently used in Match_rules.ml to extract patterns *)
let visit_sformula f formula =
  (* OK, this is only a little disgusting, but...
     Evaluation order means that we will only visit children after parents.
     So we keep a reference cell around, and set it to true whenever we descend
     under an inside.
     That way, pattern leaves underneath an Inside will properly be paired with
     a true boolean.
  *)
  let bref = ref false in
  let rec visit_sformula f formula =
    match formula with
    | Leaf p -> f p !bref
    | Not (_, x) -> visit_sformula f x
    | Inside (_, sformula) ->
        bref := true;
        visit_sformula f sformula
    | Taint (_, { sources; sinks; sanitizers; propagators }) ->
        let apply g l =
          Common.map (g (visit_sformula f)) l |> ignore;
          ()
        in
        apply visit_source (sources |> snd);
        apply visit_propagate propagators;
        apply visit_sink (sinks |> snd);
        apply visit_sanitizer sanitizers;
        ()
    | Or (_, xs) -> xs |> List.iter (visit_sformula f)
    | And (_, fand) ->
        fand.positives |> List.iter (visit_sformula f);
        fand.negatives |> List.iter (fun (_tk, x) -> visit_sformula f x)
  and visit_source f { source_formula; _ } = f source_formula
  and visit_sink f { sink_formula; _ } = f sink_formula
  and visit_propagate f { propagate_formula; _ } = f propagate_formula
  and visit_sanitizer f { sanitizer_formula; _ } = f sanitizer_formula in
  visit_sformula f formula
