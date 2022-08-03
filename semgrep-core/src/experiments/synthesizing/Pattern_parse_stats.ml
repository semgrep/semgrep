open Common
module G = AST_generic
module V = Visitor_AST
module Set = Set_

type result = Success | No_matches of string | Parse_failure of string

let add_opt x set_ref =
  let set = !set_ref in
  let set' =
    match x with
    | Some x -> Set.add x set
    | None -> set
  in
  set_ref := set'

let show_pattern file pattern =
  match Visitor_AST.range_of_any_opt pattern with
  | Some (start, end_) ->
      let { Range.start; end_ } = Range.range_of_token_locations start end_ in
      Some (String.sub file start (end_ - start + 1))
  | None -> None

let generate_patterns file ast =
  let patterns = ref Set.empty in
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kexpr =
          (fun (k, _) x ->
            add_opt (show_pattern file (G.E x)) patterns;
            k x);
      }
  in
  visitor ast;
  !patterns

let run_semgrep config lang pattern file =
  let generate_rule pattern lang =
    spf
      "rules:\n\
       - id: generated\n\
      \  pattern: |\n\
      \       %s\n\
      \  message: MATCH\n\
      \  languages: [%s]\n\
      \  severity: WARNING" pattern (Lang.to_string lang)
  in
  let rule = generate_rule pattern lang in
  Common2.with_tmp_dir (fun dir ->
      Common2.with_tmp_file ~str:rule ~ext:"txt" (fun rules_file ->
          let config =
            {
              config with
              Runner_config.lang = Some (Xlang.of_lang lang);
              rules_file;
              output_format = No_output;
              roots = [ file ];
              error_recovery = true;
              parsing_cache_dir = dir;
            }
          in
          let exn, res, _files =
            Run_semgrep.semgrep_with_raw_results_and_exn_handler config
          in
          match (exn, res.matches) with
          | None, [] ->
              No_matches
                (spf "-------- %s: No matches\n`%s`\n--------\n" file pattern)
          | Some _exn, _ ->
              Parse_failure
                (spf "-------- %s: Parse failure\n%s\n--------\n" file pattern)
          | _ -> Success))

let count_parsed_patterns config lang patterns files =
  let check_pattern (successes, no_matches, parse_failures) pattern =
    match run_semgrep config lang pattern files with
    | Success -> (successes + 1, no_matches, parse_failures)
    | No_matches x ->
        pr x;
        (successes, no_matches + 1, parse_failures)
    | Parse_failure x ->
        pr x;
        (successes, no_matches, parse_failures + 1)
  in
  List.fold_left check_pattern (0, 0, 0) patterns

(* Entry point *)

let get_pattern_parse_stats mk_config lang roots =
  let config = mk_config () in
  let roots =
    roots |> Common.map Run_semgrep.replace_named_pipe_by_regular_file
  in
  let files, _skipped = Find_target.files_of_dirs_or_files (Some lang) roots in
  let patterns_parse_rate file =
    let file_str = Common.read_file file in
    let { Parse_target.ast; skipped_tokens = _; _ } =
      Parse_target.just_parse_with_lang lang file
    in
    let patterns = generate_patterns file_str (G.Ss ast) |> Set.elements in
    count_parsed_patterns config lang patterns file
  in
  let successes, no_matches, parse_failures =
    List.fold_left
      (fun (acc_s, acc_nm, acc_pf) file ->
        pr2 file;
        let s, nm, pf = patterns_parse_rate file in
        (acc_s + s, acc_nm + nm, acc_pf + pf))
      (0, 0, 0) files
  in
  let total = successes + no_matches + parse_failures in
  let percent = successes * 100 / total in
  pr
    (spf "Success rate: %d / %d (%d%%), %d didn't match, %d failed to parse"
       successes total percent no_matches parse_failures)
