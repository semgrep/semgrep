open Js_of_ocaml
open Fpath_.Operators

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type jbool = bool Js.t
type jstring = Js.js_string Js.t

(*****************************************************************************)
(* Extern *)
(*****************************************************************************)
(* js_of_ocaml (jsoo) gives each executable its own pseudo-filesystem, which
   is problematic for browser environments because it effectively segregates
   the engine from the parsers, and means we have to write the rules and target
   everywhere. to work around this, we expose getters and setters so that we
   can have our parsers inherit their engine's mount points. This is
   effectively a no-op in node (see companion setter in ../engine/Main.ml)
*)
external set_jsoo_mountpoint : 'any Js.js_array -> unit = "set_jsoo_mountpoint"

(* jsoo gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in
   browser environments (see companion setter in semgrep.semgrep_js_shared.ml) *)
external get_jsoo_mountpoint : unit -> 'any list = "get_jsoo_mountpoint"
external set_parser_wasm_module : 'any -> unit = "set_parser_wasm_module"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let wrap_with_js_error ?(hook = None) f =
  try f () with
  | e ->
      let ee = Exception.catch e in
      (match hook with
      | Some hook -> hook ()
      | None -> ());
      Firebug.console##error (Js.string (Printexc.to_string e));
      Format.eprintf "\n%s\n%!" (Printexc.to_string e);
      (match e with
      | Js_error.Exn e ->
          let e = Js_error.to_error e in
          Firebug.console##error e##.stack
      | _ -> Printexc.print_backtrace stderr);
      Exception.reraise ee

let init_jsoo yaml_wasm_module =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Metrics_.g.payload.environment.isTranspiledJS <- true;
  (* NOTE: HTTP stuff won't work on node unless node_shared is included *)
  (* Using semgrep.parsing_languages makes the JS goes
     from 16MB to 7MB (in release mode) and from 110MB to 50MB (in dev mode)
     TODO: we should add language parsers dynamically, loading language "bundles"
     from the web on demand when one select a language in the playground.
     old: Parsing_init.init ();
  *)
  Yaml_ctypes_overrides.apply ();
  Data_init.init ();
  Libyaml_stubs_js.set_libyaml_wasm_module yaml_wasm_module

let setJsonnetParser
    (func : jstring -> AST_jsonnet.expr Tree_sitter_run.Parsing_result.t) =
  Parse_jsonnet.jsonnet_parser_ref :=
    fun file -> func (Js.string Fpath.(to_string file))

let setParsePattern (func : jbool -> jstring -> jstring -> 'a) =
  Parse_pattern.parse_pattern_ref :=
    fun print_error _options lang pattern ->
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
    fun (lang : Lang.t) (filename : Fpath.t) ->
      match lang with
      (* The Yaml and JSON parsers are embedded in the engine because it's a
         core component needed to parse rules *)
      | Lang.Yaml ->
          {
            ast = Yaml_to_generic.program filename;
            errors = [];
            skipped_tokens = [];
            inserted_tokens = [];
            tolerated_errors = [];
            stat = Parsing_stat.default_stat !!filename;
          }
      | _ ->
          func (Js.string (Lang.to_lowercase_alnum lang)) (Js.string !!filename)

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let ppf, flush =
  let b = Buffer.create 255 in
  let flush () =
    let s = Buffer.contents b in
    Buffer.clear b;
    s
  in
  (Format.formatter_of_buffer b, flush)

let console_report _src _level ~over k msgf =
  let k _ =
    Firebug.console##error (Js.string (flush ()));
    over ();
    k ()
  in
  msgf @@ fun ?header ?tags fmt ->
  ignore tags;
  match header with
  | None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
  | Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h

(*****************************************************************************)
(* Promises *)
(*****************************************************************************)

let _Promise = Js.Unsafe.global##._Promise

let promise_of_lwt lwt =
  new%js _Promise
    (Js.wrap_callback (fun resolve reject ->
         try%lwt
           let%lwt res = lwt () in
           Js.Unsafe.fun_call resolve [| Js.Unsafe.inject res |]
         with
         | e ->
             let msg = Printexc.to_string e in
             Firebug.console##error (Js.string msg);
             Js.Unsafe.fun_call reject
               [| Js.Unsafe.inject (new%js Js.error_constr (Js.string msg)) |]))

(*****************************************************************************)
(* Entrypoints *)
(*****************************************************************************)

let make_js_module ?(parse_target_ts_only = None) (langs : Language.t list)
    parse_target parse_pattern =
  let lang_names =
    Array.of_list
      (List_.map (fun x -> Js.string (Lang.to_lowercase_alnum x)) langs)
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
              (Fpath.v (Js.to_string file))
          in
          wrap_with_js_error parse_target

        method parsePattern print_errors lang str =
          let parse_pattern () =
            parse_pattern (Js.to_bool print_errors)
              (Lang.of_string (Js.to_string lang))
              (Js.to_string str)
          in
          wrap_with_js_error parse_pattern

        method parseTargetTsOnly (file : jstring) =
          let parse_target_ts_only () =
            match parse_target_ts_only with
            | Some parse_target_ts_only ->
                parse_target_ts_only (Fpath.v (Js.to_string file))
            | None ->
                failwith "parseTargetTsOnly is not supported for this language"
          in
          wrap_with_js_error parse_target_ts_only
      end)
