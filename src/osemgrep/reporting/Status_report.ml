(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from semgrep_main.py (print_scan_status()) and from
  core_runner.py (print()).
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_status ~num_rules ~num_targets ~respect_git_ignore lang_jobs ppf =
  Fmt_helpers.pp_heading ppf "Scan Status";
  Fmt.pf ppf "  Scanning %s%s with %s"
    (String_utils.unit_str num_targets "file")
    (if respect_git_ignore then " tracked by git" else "")
    (String_utils.unit_str num_rules "Code rule");

  (* TODO if sca_rules ...
     Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
  (* TODO pro_rule
         if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
         == "pro_rules"
     if pro_rule_count:
         summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
  *)
  Fmt.pf ppf ":@.";
  if num_rules = 0 then Fmt.pf ppf "  Nothing to scan."
  else if num_rules = 1 then
    Fmt.pf ppf "  Scanning %s." (String_utils.unit_str num_targets "file")
  else
    (* TODO origin table [Origin Rules] [Community N] *)
    let xlang_label = function
      | Xlang.LGeneric
      | Xlang.LRegex ->
          "<multilang>"
      | Xlang.L (l, _) -> Lang.to_lowercase_alnum l
    in
    Fmt_helpers.pp_table
      ("Language", [ "Rules"; "Files" ])
      ppf
      (lang_jobs
      |> Common.map (fun Lang_job.{ xlang; targets; rules } ->
             (xlang_label xlang, List.length rules, List.length targets))
      |> List.fold_left
           (fun acc (lang, rules, targets) ->
             match List.partition (fun (l, _) -> l = lang) acc with
             | [], others -> (lang, [ rules; targets ]) :: others
             | [ (_, [ r1; t1 ]) ], others ->
                 (lang, [ rules + r1; targets + t1 ]) :: others
             | _ -> assert false)
           []
      |> List.rev)
