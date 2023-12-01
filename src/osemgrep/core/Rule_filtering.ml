module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Filtering rules

   Translated from exclude_rules.py and some code in formatter/base.py

   TODO? do we need also to use conf.severity to filter
   matches in Output.ml as it done originally in formatter/base.py?
   But a match severity comes from a rule severity, so if the rule
   was filtered, the match should not be there anyway?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : Out.product list;
}
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let get_rule_product_from_metadata (rule : Rule.t) =
  match rule.metadata with
  | Some json -> (
      let product_field = JSON.member "product" json in
      let sca_field = JSON.member "sca-kind" json in
      match (product_field, sca_field) with
      | Some (String "secrets"), _ -> `Secrets
      | _, Some (String _) -> `SCA
      | _ -> `SAST)
  | _ -> `SAST

let filter_rules (conf : conf) (rules : Rule.rules) : Rule.rules =
  let rules =
    match conf.severity with
    | [] -> rules
    | xs -> rules |> List.filter (fun r -> List.mem r.Rule.severity xs)
  in
  rules
  |> List_.exclude (fun r -> List.mem (fst r.Rule.id) conf.exclude_rule_ids)
  |> List_.exclude (fun r ->
         List.mem (get_rule_product_from_metadata r) conf.exclude_products)
