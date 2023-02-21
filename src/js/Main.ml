open Js_of_ocaml



let _ =
  Common.jsoo := true;
  Common.debugger := true;
  Js.Unsafe.global##.Semgrep := (object%js
    method execute language rule_file source_file =
      let rule: Rule.rule = {
        id = "test";
        mode = Rule.search_mode;
        message = "foo";
        languages = "foo";
        severity = "Asdasd";
        options = {};
        
      } in
      let config : Runner_config.t =
        {
          Runner_config.default with
          rule_source = Some (Rules [rule]);
          lang = Some (Xlang.of_string (Js.to_string language));
          output_format = Json false;
          roots = [ Js.to_string source_file ];
        }
      in
      let timed_rules = (Parse_rule.parse_and_filter_invalid_rules (Js.to_string rule_file), 0.) in
      let res, files = Run_semgrep.semgrep_with_rules config timed_rules in
          let res =
            JSON_report.match_results_of_matches_and_errors
              (Some Autofix.render_fix) (List.length files) res
          in
          Output_from_core_j.string_of_core_match_results res
       (* method execute language rule_file source_file =
         let config : Runner_config.t =
           {
             Runner_config.default with
             pattern_file = Js.to_string rule_file;
             lang = Some (Xlang.of_string language);
             output_format = Json false;
             roots = [ Js.to_string source_file ];
           }
         in
         let lang = Lang.of_string language in
         let pattern, pattern_string = Run_semgrep.pattern_of_config lang config in
         let rule, rules_parse_time =
            Common.with_time (fun () ->
                let fk = Parse_info.unsafe_fake_info "" in
                let xlang = Xlang.L (lang, []) in
                let xpat =
                  Xpattern.mk_xpat
                    (Xpattern.Sem (pattern, lang))
                    (pattern_string, fk)
                in
                Rule.rule_of_xpattern xlang xpat)
          in
          let res, files =
            Run_semgrep.semgrep_with_rules config (([ rule ], []), rules_parse_time)
          in
          let json =
            JSON_report.match_results_of_matches_and_errors
              (Some Autofix.render_fix) (List.length files) res
          in
          Output_from_core_j.string_of_core_match_results json *)
    end)


  (*
  et rule_source =
    match config.rule_source with
    | Some (Rule_file file) ->
        (* useful when using process substitution, e.g.
         * semgrep-core -rules <(curl https://semgrep.dev/c/p/ocaml) ...
         *)
        Some (Rule_file (replace_named_pipe_by_regular_file file))
    | other -> other
  in
  try
    let timed_rules =
      match rule_source with
      | Some (Rule_file file) ->
          logger#linfo (lazy (spf "Parsing %s:\n%s" file (read_file file)));
          let timed_rules =
            Common.with_time (fun () ->
                Parse_rule.parse_and_filter_invalid_rules file)
          in
          timed_rules
      | Some (Rules rules) -> ((rules, []), 0.)
      | None ->
          (* TODO: ensure that this doesn't happen *)
          failwith "missing rules"
    in
    let res, files = semgrep_with_rules config timed_rules in
    (None, res, files)
  with
  | exn when not !Flag_semgrep.fail_fast ->
      let e = Exception.catch exn in
      logger#info "Uncaught exception: %s" (Exception.to_string e);
      let res =
        { RP.empty_final_result with errors = [ E.exn_to_error "" e ] }
      in
      (Some e, res, [])   
  
  *)