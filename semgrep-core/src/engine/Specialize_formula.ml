module R = Rule
module RM = Range_with_metavars
module G = AST_generic
module M = Metavariable
module PM = Pattern_match
module RP = Report

type selector = {
  mvar : M.mvar;
  pid : int;
  pstr : string;
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

let select_from_ranges file (sel_opt : selector option) (ranges : RM.ranges) :
    RM.ranges =
  let pattern_match_from_binding selector (mvar, mval) =
    {
      PM.rule_id = fake_rule_id (selector.pid, selector.pstr);
      PM.file;
      PM.range_loc = Visitor_AST.range_of_any (M.mvalue_to_any mval);
      PM.tokens = lazy (M.ii_of_mval mval);
      PM.env = [ (mvar, mval) ];
    }
  in
  let select_from_range range =
    match sel_opt with
    | None -> [ range ]
    | Some selector -> (
        match
          List.find_opt
            (fun (mvar, _mval) -> M.equal_mvar selector.mvar mvar)
            range.RM.mvars
        with
        | None -> []
        | Some binding ->
            (* make a pattern match, then use RM.match_result_to_range *)
            [
              RM.match_result_to_range
                (pattern_match_from_binding selector binding);
            ])
  in
  List.flatten (List.map select_from_range ranges)

let selector_equal s1 s2 = s1.mvar = s2.mvar

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let selector_from_formula match_func f =
  match f with
  | R.Leaf (R.P ({ pat = Sem (pattern, _); pid; pstr }, None)) -> (
      match pattern with
      | G.E (G.N (G.Id ((mvar, _), _))) ->
          Some
            {
              mvar;
              pid;
              pstr;
              lazy_matches = lazy (match_func [ (pattern, pid, pstr) ]);
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
    let _convert_and_formulas fs =
      (* TODO put back this function *)
      let selector, fs = remove_selectors (None, []) fs in
      (selector, List.map formula_to_sformula fs)
    in
    (* Visit formula and convert *)
    match formula with
    | R.Leaf leaf -> Leaf leaf
    | R.And fs -> And (None, List.map formula_to_sformula fs)
    | R.Or fs -> Or (List.map formula_to_sformula fs)
    | R.Not f -> Not (formula_to_sformula f)
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
