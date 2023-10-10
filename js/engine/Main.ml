open Js_of_ocaml
open Semgrep_js_shared

let _ =
  Js.export_all
    (object%js
       (*
          The following methods are used internally by js/engine/src/index.js.
        *)
       method init = init_jsoo
       method getMountpoints = get_jsoo_mountpoint ()
       method setParsePattern = setParsePattern
       method setJustParseWithLang = setJustParseWithLang
       method setJsonnetParser = setJsonnetParser

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
         let execute () =
           let xlang = Xlang.of_string (Js.to_string language) in
           let rules_and_errors =
             Parse_rule.parse_and_filter_invalid_rules ~rewrite_rule_ids:None
               (Fpath.v (Js.to_string rule_file))
           in
           let source_files =
             Js.to_array source_files |> Array.to_list |> List.map Js.to_string
           in
           let targets =
             List.map
               (fun f -> Input_to_core_t.{ path = f; analyzer = xlang })
               source_files
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
               (Some Autofix.render_fix) res
           in
           Semgrep_output_v1_j.string_of_core_output res
         in
         wrap_with_js_error execute
    end)
