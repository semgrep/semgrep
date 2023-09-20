open Js_of_ocaml
open Semgrep_js_shared

let _ =
  Js.export_all
    (object%js
       (*
          The following methods are used internally by js/engine/src/index.js.
        *)
       method isetLibYamlWasmModulenit = init_jsoo
       method getMountpoints = get_jsoo_mountpoint ()

       method setParsePattern (func : jbool -> jstring -> jstring -> 'a) =
         Parse_pattern.parse_pattern_ref :=
           fun print_error lang pattern ->
             match lang with
             (* The Yaml and JSON parsers are embedded in the engine because it's a
                core component needed to parse rules *)
             | Lang.Yaml -> Yaml_to_generic.any pattern
             | _ ->
                 func (Js.bool print_error)
                   (Js.string (Lang.to_lowercase_alnum lang))
                   (Js.string pattern)

       method setJustParseWithLang
           (func : jstring -> jstring -> Parsing_result2.t) =
         Parse_target.just_parse_with_lang_ref :=
           fun lang filename ->
             match lang with
             (* The Yaml and JSON parsers are embedded in the engine because it's a
                core component needed to parse rules *)
             | Lang.Yaml ->
                 {
                   ast = Yaml_to_generic.program filename;
                   errors = [];
                   skipped_tokens = [];
                   inserted_tokens = [];
                   stat = Parsing_stat.default_stat filename;
                 }
             | _ ->
                 func
                   (Js.string (Lang.to_lowercase_alnum lang))
                   (Js.string filename)

       (*
          The following methods are part of the engine's public API.
          Refer to js/engine/src/index.d.ts for more information.
        *)
       method writeFile filename content =
         Common.write_file (Js.to_string filename) (Js.to_string content)

       method deleteFile filename = Sys.remove (Js.to_string filename)

       method lookupLang lang_str =
         match Lang.of_string_opt (Js.to_string lang_str) with
         | Some lang -> Js.some (Js.string (Lang.to_lowercase_alnum lang))
         | None -> Js.null

       (* Here we take in root, and source_files, where source_files are
          the scan targets. Normally the playground will only scan one file
          and have no use for roots. The pro engine can scan multiple files
          and needs roots, so to keep the API consistent we have oss accept
          a root and multiple source_files.
       *)
       (* coupling: This is similar to semgrep_with_rules, the main
          difference being we prepare the rules and targets ourselves
          since we have a narrow use case of running one rule file on
          set files *)
       method execute language rule_file _root source_files : string =
         let xlang = Xlang.of_string (Js.to_string language) in
         let rules_and_errors =
           Parse_rule.parse_and_filter_invalid_rules ~rewrite_rule_ids:None
             (Fpath.v (Js.to_string rule_file))
         in
         let source_files =
           Js.to_array source_files |> Array.to_list |> List.map Js.to_string
         in
         let rule_ids =
           rules_and_errors |> fst |> List.map (fun r -> fst r.Rule.id)
         in
         let target_mappings =
           List.map
             (fun f ->
               Input_to_core_t.
                 {
                   path = f;
                   language = xlang;
                   rule_nums = Common.mapi (fun i _ -> i) rule_ids;
                 })
             source_files
         in
         let targets =
           Input_to_core_t.
             { target_mappings; rule_ids = Rule_ID.to_string_list rule_ids }
         in
         let config : Core_scan_config.t =
           {
             Core_scan_config.default with
             rule_source = Some (Rule_file (Fpath.v (Js.to_string rule_file)));
             output_format = Json false;
             target_source = Some (Core_scan_config.Targets targets);
             matching_explanations = true;
           }
         in
         let timed_rules = (rules_and_errors, 0.) in
         let res = Core_scan.scan config timed_rules in
         let res =
           Core_json_output.core_output_of_matches_and_errors
             (Some Autofix.render_fix) (List.length res.scanned) res
         in
         Semgrep_output_v1_j.string_of_core_output res
    end)
