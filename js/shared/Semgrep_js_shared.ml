open Js_of_ocaml

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type jbool = bool Js.t
type jstring = Js.js_string Js.t

(*****************************************************************************)
(* Extern *)
(*****************************************************************************)
(* js_of_ocaml gives each executable its own pseudo-filesystem, which is problematic for browser environments
   because it effectively segregates the engine from the parsers, and means we have to write the rules and target
   everywhere. to work around this, we expose getters and setters so that we can have our parsers
   inherit their engine's mount points. This is effectively a no-op in node.
   (see companion setter in ../engine/Main.ml) *)
external set_jsoo_mountpoint : 'any Js.js_array -> unit = "set_jsoo_mountpoint"

(* js_of_ocaml gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in browser environments
   (see companion setter in semgrep.semgrep_js_shared.ml) *)
external get_jsoo_mountpoint : unit -> 'any list = "get_jsoo_mountpoint"
external set_parser_wasm_module : 'any -> unit = "set_parser_wasm_module"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let wrap_with_js_error f =
  try f () with
  | e ->
      let e = Js_error.attach_js_backtrace e ~force:false in
      Js_error.raise_ (Option.get (Js_error.of_exn e))

let init_jsoo yaml_wasm_module =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  (* Using semgrep.parsing_languages makes the JS goes
     from 16MB to 7MB (in release mode) and from 110MB to 50MB (in dev mode)
     TODO: we should add language parsers dynamically, loading language "bundles"
     from the web on demand when one select a language in the playground.
     old: Parsing_init.init ();
  *)
  Yaml_ctypes_overrides.apply ();
  Http_helpers.client_ref := Some (module Cohttp_lwt_jsoo.Client);
  Libyaml_stubs_js.set_libyaml_wasm_module yaml_wasm_module

(*****************************************************************************)
(* Entrypoints *)
(*****************************************************************************)
let setParsePattern (func : jbool -> jstring -> jstring -> 'a) =
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

let setJustParseWithLang (func : jstring -> jstring -> Parsing_result2.t) =
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
          func (Js.string (Lang.to_lowercase_alnum lang)) (Js.string filename)

let make_js_module (langs : Lang.t list) parse_target parse_pattern =
  let lang_names =
    Array.of_list
      (Common.map (fun x -> Js.string (Lang.to_lowercase_alnum x)) langs)
  in
  Js.export "createParser" (fun wasm_module ->
      set_parser_wasm_module wasm_module;
      object%js
        method getLangs =
          let getLangs () = Js.array lang_names in
          wrap_with_js_error getLangs

        method setMountpoints mountpoints =
          let setMountpoints () = set_jsoo_mountpoint mountpoints in
          wrap_with_js_error setMountpoints

        method parseTarget lang file =
          let parse_target () =
            parse_target
              (Lang.of_string (Js.to_string lang))
              (Js.to_string file)
          in
          wrap_with_js_error parse_target

        method parsePattern print_errors lang str =
          let parse_pattern () =
            parse_pattern (Js.to_bool print_errors)
              (Lang.of_string (Js.to_string lang))
              (Js.to_string str)
          in
          wrap_with_js_error parse_pattern
      end)
