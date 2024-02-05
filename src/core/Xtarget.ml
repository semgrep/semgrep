(* eXtended target.
 *
 * This type is mostly used in the engine, to pass around extra information
 * associated to each target.
 *
 * related: Input_to_core.target, which is what is passed
 * to semgrep-core via -target.
 *)

type t = {
  source : Source.t;
  file : Fpath.t;
  xlang : Xlang.t;
  lazy_content : string lazy_t;
  (* This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
}

let parse_file parser (analyzer : Xlang.t) path =
  lazy
    (let lang =
       (* ew. We fail tests if this gets pulled out of the lazy block. *)
       match analyzer with
       | L (lang, []) -> lang
       | L (_lang, _ :: _) ->
           failwith
             "xlang from the language field in -target should be unique (this \
              shouldn't happen FIXME)"
       | _ ->
           (* alt: could return an empty program, but better to be defensive *)
           failwith "requesting generic AST for an unspecified target language"
     in
     parser lang path)

let resolve parser (target : Target_location.code) : t =
  {
    source = target.source;
    file = target.file;
    xlang = target.analyzer;
    lazy_content = lazy (UFile.read_file target.file);
    lazy_ast_and_errors = parse_file parser target.analyzer target.file;
  }
