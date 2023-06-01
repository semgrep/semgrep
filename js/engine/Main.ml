open Js_of_ocaml

(* js_of_ocaml gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in browser environments
   (see companion setter in Semgrep_js_shared.ml) *)
external get_jsoo_mount_point : unit -> 'any list = "get_jsoo_mount_point"

(* semgrep uses ocaml-yaml, which uses ocaml-ctypes to use the libyaml C library
   this is problematic for webassembly because it doesn't match the architecture
   of the build machine (e.g. most computers these days have a 64-bit address space,
   but WASM is currently 32-bit). cross-compilation ended up being too much work, so
   instead we exploit javascript's mutability to correct some memory offsets for WASM *)
external override_yaml_ctypes_field_offset :
  ('a, 'b) Yaml_bindings.T.field -> int -> unit
  = "override_yaml_ctypes_field_offset"

let fix_libyaml_field_offsets_for_wasm () =
  (* mark *)
  override_yaml_ctypes_field_offset Yaml_types.M.Mark.line 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Mark.column 8;

  (* event *)
  override_yaml_ctypes_field_offset Yaml_types.M.Event.data 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.start_mark 32;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.start_mark 44;

  (* version_directive *)
  override_yaml_ctypes_field_offset Yaml_types.M.Version_directive.minor 4;

  (* event_sequence_start *)
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Sequence_start.tag 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Sequence_start.implicit 8;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Sequence_start.style 12;

  (* event_scalar *)
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.tag 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.value 8;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.length 12;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.plain_implicit 16;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.quoted_implicit 20;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Scalar.style 24;

  (* event_mapping_start *)
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Mapping_start.tag 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Mapping_start.implicit 8;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Mapping_start.style 12;

  (* event_document_start *)
  override_yaml_ctypes_field_offset
    Yaml_types.M.Event.Document_start.tag_directives 4;
  override_yaml_ctypes_field_offset Yaml_types.M.Event.Document_start.implicit
    12

type jbool = bool Js.t
type jstring = Js.js_string Js.t

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  (* Using semgrep.parsing_languages makes the JS goes
     from 16MB to 7MB (in release mode) and from 110MB to 50MB (in dev mode)
     TODO: we should add language parsers dynamically, loading language "bundles"
     from the web on demand when one select a language in the playground.
     old: Parsing_init.init ();
  *)
  fix_libyaml_field_offsets_for_wasm ();

  Js.export_all
    (object%js
       (*
          The following methods are used internally by js/engine/src/index.js.
        *)
       method getMountpoints = get_jsoo_mount_point ()
       method setLibYamlWasmModule = Libyaml_stubs_js.set_libyaml_wasm_module

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
                   skipped_tokens = [];
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

       method execute language rule_file source_file : string =
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
