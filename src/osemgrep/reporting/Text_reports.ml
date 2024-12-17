(* Yoann Padioleau, Robur
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Text "reports".
 *
 * This module contains the code that displays summaries (on stderr)
 * as well as some reports shown usually via Logs.info and displayed
 * via --verbose (also on stderr).
 * Most functions are called from Scan_subcommand.ml
 * Those text reports are shown even when the user chose an output
 * format such as --sarif.
 *
 * Note that for the actual display of findings in text mode, see
 * Text_output.ml which follows the convention for the other output
 * format such as Sarif_output.ml.
 * Moreover, Text_output.ml is the only module displaying on stdout so
 * one can easily focus just on the finding by redirecting stderr
 * to /dev/null.
 *
 * The order of the functions in this file is mostly the order in which
 * the information is displayed to the user in the terminal:
 *  - running rules (Logs.info)
 *  - roots, skipped part1, and selected targets (Log_targeting.Log.debug)
 *  - TODO Code/SCA/Secret rules, language/origin, targets (Logs.app)
 * # (Findings in Text_output.ml/Sarif_output.ml/... on stdout)
 *  - File skipped part2 (Logs.info)
 *  - Scan summary (Logs.app)
 *
 * Partially translated from:
 *  - formatter/text.py ??
 *  - output.py ??
 *  - semgrep_main.py ??
 *  - core_runner.py ??
 *  - target_manager.py (for skipped, done via some 'yield' in Python)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let rule_id r = fst r.Rule.id

(* by rule id *)
let compare_rule r1 r2 = Rule_ID.compare (rule_id r1) (rule_id r2)

(* by path *)
let compare_skipped_target (a : Out.skipped_target) (b : Out.skipped_target) =
  Fpath.compare a.path b.path

(* by ?? *)
let compare_lang_job (lang, rules_targets) (lang', rules_targets') =
  match (rules_targets, rules_targets') with
  | [ rules; targets ], [ rules'; targets' ] -> (
      match -compare targets targets' with
      | 0 -> (
          match -compare rules rules' with
          | 0 -> compare lang lang'
          | cmp -> cmp)
      | cmp -> cmp)
  | _ -> failwith "Unexpected pattern"

let bold s = Console.sprintf [ Console.Bold ] "%s" s
let cyan s = Console.sprintf [ Console.cyan ] "%s" s

let opt_msg msg = function
  | [] -> None
  | xs -> Some (string_of_int (List.length xs) ^ " " ^ msg)

(* NOTE: Some "languages" are instead general-purpose text analyzers and not
 * true programming languages. These include "regex", "generic" AKA
 * "spacegrep", and "aliengrep".
 * Each of these "languages" have their own pattern syntax
 * and engine for matching patterns against targets, and thus need to be
 * executed separately from each other. However, for simplicity, we merge
 * the stats for these "languages" into a single "<multilang>" row.
 *)
let xlang_label = function
  | Xlang.LSpacegrep
  | Xlang.LAliengrep
  | Xlang.LRegex ->
      "<multilang>"
  | Xlang.L (l, _) -> Lang.to_lowercase_alnum l

(*****************************************************************************)
(* Targets debug *)
(*****************************************************************************)

(* not in pysemgrep and currently enabled only via SEMGREP_LOG_SRCS=targeting*)
let targets (roots : Scanning_root.t list)
    (skipped : Semgrep_output_v1_t.skipped_target list) (targets : Fpath.t list)
    : string =
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      prf "target roots: [\n";
      roots
      |> List.iter (fun root -> prf "  %s\n" !!(Scanning_root.to_fpath root));
      prf "]\n";
      prf "skipped targets: [\n";
      skipped
      |> List.iter (fun x ->
             prf "  %s" (Semgrep_output_v1_t.show_skipped_target x));
      prf "]\n";
      prf "selected targets: [\n";
      targets |> List.iter (fun file -> prf "target = %s\n" !!file);
      prf "]\n";
      (* more info about skipped targets *)
      skipped
      |> List.iter (fun (x : Semgrep_output_v1_t.skipped_target) ->
             prf "Ignoring %s due to %s (%s)" !!(x.path)
               (Semgrep_output_v1_t.show_skip_reason x.reason)
               (x.details ||| "")))

(*****************************************************************************)
(* Rules info *)
(*****************************************************************************)

let rules ~too_many_entries (src : Rules_source.t) (rules : Rule.t list) :
    string =
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      prf "running %d rules from %s\n" (List.length rules)
        (match src with
        | Pattern _ -> "pattern"
        | Configs [ x ] -> spf "1 config %s" x
        | Configs xs -> spf "%d configs" (List.length xs));

      (* TODO should output whether .semgrepignore is found and used
         (as done in semgrep_main.py get_file_ignore()) *)
      prf "Rules:\n";
      let experimental, normal =
        rules
        |> List.partition (fun (rule : Rule.t) -> rule.severity =*= `Experiment)
      in
      if too_many_entries > 0 && List.length normal > too_many_entries then
        prf "%s" Output.too_much_data
      else
        normal |> List.sort compare_rule
        |> List.iter (fun rule ->
               prf "- %s\n" (Rule_ID.to_string (rule_id rule)));
      if not (List_.null experimental) then begin
        prf "Experimental rules:\n";
        experimental |> List.sort compare_rule
        |> List.iter (fun rule ->
               prf "- %s\n" (Rule_ID.to_string (rule_id rule)))
      end)

(*****************************************************************************)
(* Product x rules x targets x languages *)
(*****************************************************************************)

(*
  Partially translated from semgrep_main.py (print_scan_status()) and from
  core_runner.py (print()).
*)
let origin rule =
  (match rule.Rule.metadata with
  | Some (Object _ as meta) -> (
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
  ||| "custom"

let scan_status ~num_rules ~num_targets ~respect_gitignore
    (lang_jobs : Lang_job.t list) : string =
  ignore (num_rules, num_targets, respect_gitignore, lang_jobs, origin);
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in

      (* TODO: seems like pysemgrep does not show this heading anymore *)
      prf "%s" (Console.heading "Scan Status");

      prf "  Scanning %s%s with %s"
        (String_.unit_str num_targets "file")
        (* TODO: validate if target is actually within a git repo *)
        (if respect_gitignore then " tracked by git" else "")
        (String_.unit_str num_rules "Code rule");

      (* TODO if sca_rules ...
         Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
      (* TODO pro_rule
             if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
             == "pro_rules"
         if pro_rule_count:
             summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
      *)
      prf ":\n";

      match num_rules with
      | 0 -> prf "  Nothing to scan."
      | 1 -> prf "  Scanning %s." (String_.unit_str num_targets "file")
      | _else_ ->
          let rule_origins : (string * int list) list =
            lang_jobs
            |> List.fold_left
                 (fun acc Lang_job.{ rules; _ } -> List_.map origin rules @ acc)
                 []
            |> Assoc.group_by Fun.id
            |> List_.map (fun (src, xs) ->
                   (String.capitalize_ascii src, [ List.length xs ]))
          in
          prf "\n";
          let lang_stats : (string * int * int) list =
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
          prf "%s"
            (Console.tables
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
                 |> List.sort compare_lang_job )
               ("Origin", [ "Rules" ], rule_origins)))

(*****************************************************************************)
(* Findings/Matches *)
(*****************************************************************************)
(* See Text_output.ml/Sarif_output.ml/... *)

(*****************************************************************************)
(* Skipped *)
(*****************************************************************************)

(* Partially translated from target_manager.py (yield_verbose_lines()) *)
let skipped ~too_many_entries ~respect_git_ignore ~max_target_bytes
    (maturity : Maturity.t) (groups : Skipped_groups.t) : string =
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      let prf_list (xs : Out.skipped_target list) =
        match xs with
        | [] -> prf "   • <none>\n"
        | xs ->
            xs
            |> List.sort compare_skipped_target
            |> List.iter (fun ({ path; _ } : Out.skipped_target) ->
                   prf "   • %s\n" (cyan !!path))
      in

      (* TODO: Why pysemgrep does not use the classic heading for skipped?? *)
      (* nope: Fmt_helpers.pp_heading ppf "Files skipped"; *)
      prf "\n%s\nFiles skipped:\n%s\n\n" (String.make 40 '=')
        (String.make 40 '=');

      prf "  %s\n\n" (bold "Always skipped by Semgrep:");
      prf_list groups.always;
      prf "\n";
      prf "  %s\n" (bold "Skipped by .gitignore:");
      if respect_git_ignore then begin
        prf "  %s\n\n" (bold "(Disable by passing --no-git-ignore)");
        prf "   • <all files not listed by `git ls-files` were skipped>\n"
      end
      else begin
        prf "  %s\n\n" (bold "(Disabled with --no-git-ignore)");
        prf "   • <none>\n"
      end;
      prf "\n";

      prf "  %s\n  %s\n\n"
        (bold "Skipped by .semgrepignore:")
        (bold
           "(See: \
            https://semgrep.dev/docs/ignoring-files-folders-code/#understand-semgrep-defaults)");
      prf_list groups.ignored;
      prf "\n";

      prf "  %s\n\n" (bold "Skipped by --include patterns:");
      prf_list groups.include_;
      prf "\n";

      prf "  %s\n\n" (bold "Skipped by --exclude patterns:");
      if too_many_entries > 0 && List.length groups.exclude > too_many_entries
      then prf "   • %s\n" Output.too_much_data
      else prf_list groups.exclude;
      prf "\n";

      prf "  %s\n  %s\n\n"
        (bold
           (spf "Skipped by limiting to files smaller than %d bytes:"
              max_target_bytes))
        (bold "(Adjust with the --max-target-bytes flag)");
      prf_list groups.size;
      prf "\n";

      (match maturity with
      | Maturity.Develop ->
          prf "  %s\n\n" (bold "Skipped for other reasons:");
          prf_list groups.other;
          prf "\n"
      | _else_ -> ());

      prf "  %s\n\n"
        (bold "Partially analyzed due to parsing or internal Semgrep errors");
      prf_list groups.errors;
      prf "\n")

(*****************************************************************************)
(* Scan summary *)
(*****************************************************************************)
let scan_summary ~respect_gitignore ~max_target_bytes ~num_valid_rules
    (maturity : Maturity.t) (cli_output : Out.cli_output)
    (groups : Skipped_groups.t) : string =
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      prf "%s" (Console.heading "Scan Summary");
      (* TODO
            if self.target_manager.baseline_handler:
                limited_fragments.append(
                    "Scan was limited to files changed since baseline commit."
                )
      *)
      let out_limited =
        if respect_gitignore then
          (* # Each target could be a git repo, and we respect the git ignore
             # of each target, so to be accurate with this print statement we
             # need to check if any target is a git repo and not just the cwd
             targets_not_in_git = 0
             dir_targets = 0
             for t in self.target_manager.targets:
                 if t.path.is_dir():
                     dir_targets += 1
                     try:
                         t.files_from_git_ls()
                     except (subprocess.SubprocessError, FileNotFoundError):
                         targets_not_in_git += 1
                         continue
             if targets_not_in_git != dir_targets: *)
          Some "Scan was limited to files tracked by git."
        else None
      in
      let out_skipped =
        let mb = string_of_int Stdlib.(max_target_bytes / 1000 / 1000) in
        [
          opt_msg "files not matching --include patterns" groups.include_;
          opt_msg "files matching --exclude patterns" groups.exclude;
          opt_msg ("files larger than " ^ mb ^ " MB") groups.size;
          opt_msg "files matching .semgrepignore patterns" groups.ignored;
          (match maturity with
          | Develop -> opt_msg "other files ignored" groups.other
          | Default
          | Legacy
          | Experimental ->
              None);
        ]
        |> List_.filter_map Fun.id
      in
      let out_partial =
        opt_msg
          "files only partially analyzed due to a parsing or internal Semgrep \
           error"
          groups.errors
      in
      (match (out_skipped, out_partial, out_limited, groups.ignored) with
      | [], None, None, [] -> ()
      | xs, parts, limited, _ignored -> (
          prf "Some files were skipped or only partially analyzed.\n";
          limited |> Option.iter (fun txt -> prf "  %s\n" txt);
          parts |> Option.iter (fun txt -> prf "  Partially scanned: %s\n" txt);
          match xs with
          | [] -> ()
          | xs ->
              prf "  Scan skipped: %s.\n" (String.concat ", " xs);
              prf
                "  For a full list of skipped files, run semgrep with the \
                 --verbose flag.\n"));
      prf "\n";
      prf "Ran %s on %s: %s."
        (String_.unit_str num_valid_rules "rule")
        (String_.unit_str (List.length cli_output.paths.scanned) "file")
        (String_.unit_str (List.length cli_output.results) "finding"))
