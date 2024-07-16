open Js_of_ocaml
(* coupling: with js/engine/src/index.d.ts *)

let _ =
  Js.export_all
    (object%js
       (*
          The following methods are used internally by js/engine/src/index.js.
        *)
       method init = Semgrep_js_shared.init_jsoo
       method getMountpoints = Semgrep_js_shared.get_jsoo_mountpoint ()
       method setParsePattern = Semgrep_js_shared.setParsePattern
       method setJustParseWithLang = Semgrep_js_shared.setJustParseWithLang
       method setJsonnetParser = Semgrep_js_shared.setJsonnetParser

       (*
          The following methods are part of the engine's public API.
          Refer to js/engine/src/index.d.ts for more information.
        *)
       method writeFile filename content =
         UFile.Legacy.write_file (Js.to_string filename) (Js.to_string content)

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
          a set of files *)
       method execute language rule_file _root source_files : string =
         let execute () =
           let xlang = Xlang.of_string (Js.to_string language) in
           let rule_file = Fpath.v (Js.to_string rule_file) in
           let rules_and_errors =
             Parse_rule.parse_and_filter_invalid_rules ~rewrite_rule_ids:None
               rule_file
           in
           let source_files =
             Js.to_array source_files |> Array.to_list |> List.map Js.to_string
           in
           let targets =
             List.map
               (fun f : Target.t ->
                 Regular
                   (Target.mk_regular xlang Product.all (File (Fpath.v f))))
               source_files
           in
           let default_config = Output.default in
           let config : Core_scan_config.t =
             {
               Core_scan_config.default with
               rule_source = Some (Rule_file rule_file);
               output_format = Json false;
               target_source = Some (Core_scan_config.Targets targets);
               matching_explanations = true;
             }
           in
           (* Core_scan.scan needs /tmp just to handle Git_remove rules_source,
            * but this should not happen here, so it's ok to use tmp_caps_UNSAFE
            *)
           let caps = Cap.tmp_caps_UNSAFE () in
           let res_or_exn = Core_scan.scan_with_exn_handler caps config in
           let res =
             match res_or_exn with
             | Error (e, _core_error_opt) ->
                 (* TODO? maybe we should Exception.reraise e instead, but then need
                  * to fix some regressions in make -C js test
                  *)
                 let err = Core_error.exn_to_error None "" e in
                 Core_result.mk_final_result_with_just_errors [ err ]
             | Ok res -> res
           in
           let res =
             match rules_and_errors with
             | Ok (rules, _) -> Core_runner.create_core_result rules res
             | Error e ->
                 Core_runner.create_core_result []
                   (Core_result.mk_final_result_with_just_errors
                      [
                        Core_error.error_of_rule_error
                          ~file:Fpath_.(!!rule_file)
                          e;
                      ])
           in
           (* This is just the default configuration, but this function
              doesn't actually depend upon the parts of the config that we
              set above.
           *)
           let cli_output = Output.preprocess_result default_config res in
           Semgrep_output_v1_j.string_of_cli_output cli_output
         in
         Semgrep_js_shared.wrap_with_js_error execute
    end)
