module R = Rule
module RM = Range_with_metavars
module G = AST_generic
module M = Metavariable
module PM = Pattern_match
module RP = Report

type selector = {
  mvar : M.mvar;
  pattern : AST_generic.any;
  pid : int;
  pstr : string R.wrap;
  lazy_matches : RP.times RP.match_result Lazy.t;
}

type sformula =
  | Leaf of R.leaf
  | And of (selector option * sformula list)
  | Or of sformula list
  (* There are restrictions on where a Not can appear in a formula. It
   * should always be inside an And to be intersected with "positive" formula.
   *)
  | Not of sformula

(*****************************************************************************)
(* Selecting methods *)
(*****************************************************************************)

(* this will be adjusted later in range_to_pattern_match_adjusted *)
let fake_rule_id (id, str) =
  { PM.id = string_of_int id; pattern_string = str; message = "" }

let match_selector ?err:(msg = "no match") (sel_opt : selector option) :
    RM.ranges =
  let get_match (x : RP.times RP.match_result) = x.matches in
  match sel_opt with
  | None -> failwith msg
  | Some selector ->
      List.map RM.match_result_to_range
        (get_match (Lazy.force selector.lazy_matches))

let selector_equal s1 s2 = s1.mvar = s2.mvar

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let selector_from_formula match_func f =
  match f with
  | R.Leaf (R.P ({ pat = Sem (pattern, _); pid; pstr }, None)) -> (
      match pattern with
      | G.E { e = G.N (G.Id ((mvar, _), _)); _ } ->
          Some
            {
              mvar;
              pattern;
              pid;
              pstr;
              lazy_matches = lazy (match_func [ (pattern, pid, fst pstr) ]);
            }
      | _ -> None)
  | _ -> None

let formula_to_sformula match_func formula =
  let rec formula_to_sformula formula =
    let rec remove_selectors (selector, acc) formulas =
      match formulas with
      | [] -> (selector, acc)
      | x :: xs ->
          let selector, acc =
            match (selector, selector_from_formula match_func x) with
            | None, None -> (None, x :: acc)
            | Some s, None -> (Some s, x :: acc)
            | None, Some s -> (Some s, acc)
            | Some s1, Some s2 ->
                if selector_equal s1 s2 then (Some s1, acc) else (None, [])
            (* This will never be valid *)
          in
          remove_selectors (selector, acc) xs
    in
    let convert_and_formulas fs =
      let selector, fs = remove_selectors (None, []) fs in
      (selector, List.map formula_to_sformula fs)
    in
    (* Visit formula and convert *)
    match formula with
    | R.Leaf leaf -> Leaf leaf
    | R.And (_, fs) -> And (convert_and_formulas fs)
    | R.Or (_, fs) -> Or (List.map formula_to_sformula fs)
    | R.Not (_, f) -> Not (formula_to_sformula f)
  in
  formula_to_sformula formula

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
(* currently used in Match_rules.ml to extract patterns *)
let rec visit_sformula f formula =
  match formula with
  | Leaf (P (p, _)) -> f p
  | Leaf (MetavarCond _) -> ()
  | Not x -> visit_sformula f x
  | Or xs | And (_, xs) -> xs |> List.iter (visit_sformula f)
