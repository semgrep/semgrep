(* This module contains a lot of functionality used in turbo mode, lsp.js, parsers and unit tests *)
(* In the dune file we include some c/js bindings that the core engine uses *)

type jbool = bool Js_of_ocaml.Js.t
(** [jbool] is a javascript boolean *)

type jstring = Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
(** [jstring] is a javascript string *)

(* js_of_ocaml gives each executable its own pseudo-filesystem, which is problematic for browser environments
   because it effectively segregates the engine from the parsers, and means we have to write the rules and target
   everywhere. to work around this, we expose getters and setters so that we can have our parsers
   inherit their engine's mount points. This is effectively a no-op in node.
   (see companion setter in ../engine/Main.ml) *)
external set_jsoo_mountpoint : 'any Js_of_ocaml.Js.js_array -> unit
  = "set_jsoo_mountpoint"
(** [set_jsoo_mountpoint js_obj] is a javascript function that sets the mountpoint
    of the current executable for filesystem access *)

(* js_of_ocaml gives each executable its own pseudo-filesystem, which means we must
   expose the engine's mount points in order for reads to work properly in browser environments
   (see companion setter in semgrep.semgrep_js_shared.ml) *)
external get_jsoo_mountpoint : unit -> 'any list = "get_jsoo_mountpoint"
(** [get_jsoo_mountpoint js_obj] is a javascript function that gets the mountpoint
    of the current executable for filesystem access *)

external set_parser_wasm_module : 'any -> unit = "set_parser_wasm_module"
(** [set_parser_wasm_module js_obj] is a javascript function that sets the wasm module
    of the current javascript runtime for parsing *)

val wrap_with_js_error : (unit -> 'a) -> 'a
(** [wrap_with_js_error f] wraps a function with a try-catch block
    and throws a javascript error if an exception is raised *)

val init_jsoo : 'a -> unit
(** [init_jsoo]  initializes various semgrep things for js_of_ocaml *)

val setParsePattern : (jbool -> jstring -> jstring -> AST_generic.any) -> unit
(** [setParsePattern f] is sets the parse pattern function for semgrep.
    This is what determines which parser to use for a given language pattern.
  *)

val setJustParseWithLang : (jstring -> jstring -> Parsing_result2.t) -> unit
(** [setJustParseWithLang f] is sets the parse function for semgrep.
    This is what determines which parser to use for a given language target.
  *)

val make_js_module :
  Language.t list ->
  (Language.t -> string -> 't8) ->
  (bool -> Language.t -> string -> 't9) ->
  unit
(** [make_js_module langs parse parse_pattern] creates a javascript module
    that can be used to parse a given language. It takes a list of languages, a parse function,
    and a parse_pattern function. *)
