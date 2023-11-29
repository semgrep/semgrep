(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from semgrep_main.py (print_scan_status()) and from
  core_runner.py (print()).
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let origin rule =
  Option.value ~default:"custom"
    (match rule.Rule.metadata with
    | Some meta -> (
        match Yojson.Basic.Util.member "semgrep.dev" (JSON.to_yojson meta) with
        | `Assoc _ as things -> (
            match Yojson.Basic.Util.member "rule" things with
            | `Assoc _ as things -> (
                match Yojson.Basic.Util.member "origin" things with
                | `String s -> Some s
                | _else -> None)
            | _else -> None)
        | _else -> None)
    | _else -> None)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_status ~num_rules ~num_targets ~respect_git_ignore lang_jobs ppf =
  Fmt_.pp_heading ppf "Scan Status";
  Fmt.pf ppf "  Scanning %s%s with %s"
    (String_.unit_str num_targets "file")
    (* TODO: validate if target is actually within a git repo *)
    (if respect_git_ignore then " tracked by git" else "")
    (String_.unit_str num_rules "Code rule");

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
    Fmt.pf ppf "  Scanning %s." (String_.unit_str num_targets "file")
  else
    let rule_origins =
      lang_jobs
      |> List.fold_left
           (fun acc Lang_job.{ rules; _ } -> List_.map origin rules @ acc)
           []
      |> Assoc.group_by Fun.id
      |> List_.map (fun (src, xs) ->
             (String.capitalize_ascii src, [ List.length xs ]))
    in
    Fmt.pf ppf "@.";
    let compare (lang, rules_targets) (lang', rules_targets') =
      match (rules_targets, rules_targets') with
      | [ rules; targets ], [ rules'; targets' ] -> (
          match -compare targets targets' with
          | 0 -> (
              match -compare rules rules' with
              | 0 -> compare lang lang'
              | cmp -> cmp)
          | cmp -> cmp)
      | _ -> failwith "Unexpected pattern"
    in
    (* NOTE: Some "languages" are instead general-purpose text analyzers and not true
     * programming languages. These include "regex", "generic" AKA "spacegrep",
     * and "aliengrep". Each of these "languages" have their own pattern syntax
     * and engine for matching patterns against targets, and thus need to be
    * executed separately from each other. However, for simplicity, we merge the
    * stats for these "languages" into a single "<multilang>" row.
     *)
    let xlang_label = function
      | Xlang.LSpacegrep
      | Xlang.LAliengrep
      | Xlang.LRegex ->
          "<multilang>"
      | Xlang.L (l, _) -> Lang.to_lowercase_alnum l
    in
    let lang_stats =
      lang_jobs
      (* Unpack each job, transforming xlang into its mapped language key *)
      |> List_.map (fun Lang_job.{ xlang; targets; rules } ->
             (xlang_label xlang, rules, targets))
      (* Merge jobs by mapped language key *)
      |> Assoc.group_by (fun (xlang, _, _) -> xlang)
      |> List_.map (fun (xlang, xxs) ->
             let targets =
               xxs
               |> List.concat_map (fun (_, _, targets) -> targets)
               |> Assoc.group_by Fun.id
               |> List_.map (fun (target, _) -> target)
               |> List.length
             in
             let rules =
               xxs
               |> List.concat_map (fun (_, rules, _) -> rules)
               |> Assoc.group_by Fun.id
               |> List_.map (fun (rules, _) -> rules)
               |> List.length
             in
             (xlang, rules, targets))
    in
    Fmt_.pp_tables ppf
      ( "Language",
        [ "Rules"; "Files" ],
        lang_stats
        |> List.fold_left
             (fun acc (lang, rules, targets) ->
               match List.partition (fun (l, _) -> l = lang) acc with
               | [], others -> (lang, [ rules; targets ]) :: others
               | [ (_, [ r1; t1 ]) ], others ->
                   (lang, [ rules + r1; targets + t1 ]) :: others
               | _ -> assert false)
             []
        (* Sort by files desc, rules desc, lang asc *)
        |> List.sort compare )
      ("Origin", [ "Rules" ], rule_origins)
