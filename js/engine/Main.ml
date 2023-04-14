open Js_of_ocaml

(* js_of_ocaml gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in browser environments
   (see companion setter in Semgrep_js_shared.ml) *)
external get_jsoo_mount_point : unit -> 'any list = "get_jsoo_mount_point"
external jsoo_create_file : string -> string -> unit = "jsoo_create_file"

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  (* Using semgrep.parsing_languages makes the JS goes
     from 16MB to 7MB (in release mode) and from 110MB to 50MB (in dev mode)
     TODO: we should add language parsers dynamically, loading language "bundles"
     from the web on demand when one select a language in the playground.
     old: Parsing_init.init ();
  *)
  Js.export_all
    (object%js
       method getMountPoints = get_jsoo_mount_point ()
       method setLibYamlWasmModule = Libyaml_stubs_js.set_libyaml_wasm_module
       method writeFile = jsoo_create_file

       method setParsePattern (func : bool -> Lang.t -> string -> 'a) =
         Parse_pattern.parse_pattern_ref := func

       method setJustParseWithLang
           (func : Lang.t -> string -> Parsing_result2.t) =
         Parse_target.just_parse_with_lang_ref := func

       method lookupLang (lang : Js.js_string Js.t) : Lang.t =
         Lang.of_string (Js.to_string lang)

       method execute (language : Js.js_string Js.t)
           (rule_file : Js.js_string Js.t) (source_file : Js.js_string Js.t)
           : string =
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
