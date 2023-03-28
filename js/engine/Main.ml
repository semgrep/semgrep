open Js_of_ocaml

let _ =
  Common.jsoo := true;
  (* Using semgrep.parsing_languages makes the JS goes
     from 16MB to 7MB (in release mode) and from 110MB to 50MB (in dev mode)
     TODO: we should add language parsers dynamically, loading language "bundles"
     from the web on demand when one select a language in the playground.
     old: Parsing_init.init ();
  *)
  Js.export "Semgrep"
    (object%js
       method addLanguage parseFile parseString =
         let parse_string_wrapper _ _ x = parseString x in
         Parse_pattern.parse_pattern_ref := parse_string_wrapper;
         let parse_file_wrapper _ x = parseFile x in
         Parse_target.just_parse_with_lang_ref := parse_file_wrapper

       method execute language rule_file source_file =
         let config : Runner_config.t =
           {
             Runner_config.default with
             rule_source = Some (Rule_file (Fpath.v (Js.to_string rule_file)));
             lang = Some (Xlang.of_string (Js.to_string language));
             output_format = Json false;
             roots = [ Fpath.v (Js.to_string source_file) ];
           }
         in
         let timed_rules =
           ( Parse_rule.parse_and_filter_invalid_rules
               (Fpath.v (Js.to_string rule_file)),
             0. )
         in
         let res, files = Run_semgrep.semgrep_with_rules config timed_rules in
         let res =
           JSON_report.match_results_of_matches_and_errors
             (Some Autofix.render_fix) (List.length files) res
         in
         Output_from_core_j.string_of_core_match_results res
    end)
