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
  severity : Severity.rule_severity list;
}
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let filter_rules (conf : conf) (rules : Rule.rules) : Rule.rules =
  let rules =
    match conf.severity with
    | [] -> rules
    | xs ->
        rules
        |> List.filter (fun r ->
               match
                 Severity.rule_severity_of_rule_severity_opt r.Rule.severity
               with
               | None -> false
               | Some x -> List.mem x xs)
  in
  rules
  |> Common.exclude (fun r -> List.mem (fst r.Rule.id) conf.exclude_rule_ids)
