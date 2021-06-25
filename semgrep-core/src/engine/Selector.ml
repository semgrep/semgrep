module R = Rule
module AR = Augmented_range

type selector = { metavariable : Metavariable.mvar }

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
  match sel_opt with None -> range | Some _selector -> failwith "todo"

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)

let rec formula_to_sformula formula =
  match formula with
  | R.Leaf leaf -> Leaf leaf
  | R.And fs -> And (None, List.map formula_to_sformula fs) (* Emma TODO *)
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
