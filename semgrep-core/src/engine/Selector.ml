module R = Rule
module AR = Augmented_range
module G = AST_generic

type selector = { mvar : Metavariable.mvar; token : Parse_info.t }

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

let select_from_range (sel_opt : selector option) (range : AR.range_with_mvars)
    : AR.range_with_mvars =
  match sel_opt with
  | None -> range
  | Some selector -> failwith ("todo " ^ selector.mvar)

let selector_equal s1 s2 = s1.mvar = s2.mvar

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let rec formula_to_sformula formula =
  let selector_from_formula f =
    match f with
    | R.Leaf (R.P ({ pat = Sem (pattern, _); _ }, None)) -> (
        match pattern with
        | G.E (G.N (G.Id ((mvar, token), _))) -> Some { mvar; token }
        | _ -> None )
    | _ -> None
  in
  let rec remove_selectors (selector, acc) formulas =
    match formulas with
    | [] -> (selector, acc)
    | x :: xs ->
        let selector, acc =
          match (selector, selector_from_formula x) with
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
  | R.And fs -> And (convert_and_formulas fs)
  | R.Or fs -> Or (List.map formula_to_sformula fs)
  | R.Not f -> Not (formula_to_sformula f)

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
