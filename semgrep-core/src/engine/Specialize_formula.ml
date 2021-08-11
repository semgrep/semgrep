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

let selector_equal s1 s2 = s1.mvar = s2.mvar

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let selector_from_formula f =
  match f with
  | R.Leaf (R.P ({ pat = Sem (pattern, _); pid; pstr }, None)) -> (
      match pattern with
      | G.E { e = G.N (G.Id ((mvar, _), _)); _ } ->
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
    let convert_and_formulas fs =
      let selector, fs' = remove_selectors (None, []) fs in
      (* We only want a selector if there is something to select from. *)
      match fs' with
      | [] -> (None, List.map formula_to_sformula fs)
      | _ :: _ -> (selector, List.rev_map formula_to_sformula fs')
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
