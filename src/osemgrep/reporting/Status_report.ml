(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from semgrep_main.py (print_scan_status())
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_status ~num_rules ~num_targets ~respect_git_ignore lang_jobs ppf =
  Fmt_helpers.pp_heading ppf "Scan status";
  (* TODO indentation of the body *)
  let pp_s ppf x = if x = 1 then Fmt.string ppf "" else Fmt.string ppf "s" in

  Fmt.pf ppf "Scanning %d files%s with %d Code rule%a" num_targets
    (if respect_git_ignore then " tracked by git" else "")
    num_rules pp_s num_rules;
  (* TODO if sca_rules ...
     Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
  (* TODO pro_rule
         if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
         == "pro_rules"
     if pro_rule_count:
         summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
  *)
  Fmt.pf ppf ":@.@.";
  (* TODO origin table [Origin Rules] [Community N] *)
  Fmt_helpers.pp_table
    ("Language", [ "Rules"; "Files" ])
    ppf
    (lang_jobs
    |> List.fold_left
         (fun acc Lang_job.{ lang; targets; rules } ->
           (Xlang.to_string lang, [ List.length rules; List.length targets ])
           :: acc)
         []
    |> List.rev)
