(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reporting the config and rules used to the user *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pp_rule_sources ppf = function
  | Rules_source.Pattern _ -> Format.pp_print_string ppf "pattern"
  | Configs [ x ] -> Format.fprintf ppf "1 config %s" x
  | Configs xs -> Format.fprintf ppf "%d configs" (List.length xs)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_rules ppf (rules_source, filtered_rules) =
  Fmt.pf ppf "running %d rules from %a@."
    (List.length filtered_rules)
    pp_rule_sources rules_source;
  (* TODO should output whether .semgrepignore is found and used
     (as done in semgrep_main.py get_file_ignore()) *)
  Fmt.pf ppf "Rules:@.";
  let exp, normal =
    filtered_rules
    |> List.partition (fun rule -> rule.Rule.severity = Rule.Experiment)
  in

  let rule_id r = fst r.Rule.id in
  let sorted =
    List.sort (fun r1 r2 -> Rule_ID.compare (rule_id r1) (rule_id r2))
  in
  sorted normal
  |> List.iter (fun rule ->
         Fmt.pf ppf "- %s@." (Rule_ID.to_string (rule_id rule)));
  match exp with
  | [] -> ()
  | __non_empty__ ->
      Fmt.pf ppf "Experimental rules:@.";
      sorted exp
      |> List.iter (fun rule ->
             Fmt.pf ppf "- %s@." (Rule_ID.to_string (rule_id rule)))
