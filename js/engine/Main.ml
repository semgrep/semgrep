open Js_of_ocaml

(* js_of_ocaml gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in browser environments
   (see companion setter in Semgrep_js_shared.ml) *)
external get_jsoo_mount_point : unit -> 'any list = "get_jsoo_mount_point"

(* Overrides are required because we don't have a 1:1 correspondence between Lang.t and parsers
   For example, the Python parser handles all of the Python Langs *)
let parser_lang_overrides =
  Common.hash_of_list
    [
      ("python2", "python");
      ("python3", "python");
      ("py", "python");
      ("xml", "html");
      ("scheme", "lisp");
      ("clojure", "lisp");
    ]

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
       method getMountpoints = get_jsoo_mount_point ()
       method setLibYamlWasmModule = Libyaml_stubs_js.set_libyaml_wasm_module

       method writeFile filename content =
         Common.write_file (Js.to_string filename) (Js.to_string content)

       method deleteFile filename = Sys.remove (Js.to_string filename)

       method setParsePattern
           (func : bool -> Js.js_string Js.t -> Js.js_string Js.t -> 'a) =
         Parse_pattern.parse_pattern_ref :=
           fun print_error lang target ->
             func print_error
               (lang |> Lang.to_lowercase_alnum |> String.lowercase_ascii
              |> Js.string)
               (Js.string target)

       method setJustParseWithLang
           (func : Js.js_string Js.t -> Js.js_string Js.t -> Parsing_result2.t)
           =
         Parse_target.just_parse_with_lang_ref :=
           fun lang pattern ->
             func
               (lang |> Lang.to_lowercase_alnum |> String.lowercase_ascii
              |> Js.string)
               (Js.string pattern)

       method getParserForLang lang_js_str =
         let lang_str = Js.to_string lang_js_str in
         match Hashtbl.find_opt parser_lang_overrides lang_str with
         | Some parser -> Js.some (Js.string parser)
         | None -> (
             match Lang.of_string_opt lang_str with
             | Some lang ->
                 Js.some
                   (Js.string
                      (String.lowercase_ascii (Lang.to_lowercase_alnum lang)))
             | None -> Js.null)

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
