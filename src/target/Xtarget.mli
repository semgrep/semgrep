(** eXtended target.

   This type is mostly used in the engine to pass around extra information
   (e.g., contents, the AST) associated with each {{!Target.regular}target}.
 *)

type t = {
  path : Target.path;
  analyzer : Analyzer.t;  (** The analyzer to use when scanning this target. *)
  lazy_content : string lazy_t;
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
      (** This is valid only for analyzer = Analyzer.L ..., not for LRegex|LGeneric *)
}

val resolve :
  (Language.t -> Fpath.t -> AST_generic.program * Tok.location list) ->
  Target.regular ->
  t
(** [resolve parser target] is the extended version of [target], comprising
    also the contents and parsed version thereof. *)

(* Instead of a parser (like with resolve), pass a lazy AST. This allows for the
 * easy construction of Xtargets in contexts where the client has already parsed
 * the file in question. *)
val resolve_with_ast :
  (AST_generic.program * Tok.location list) Lazy.t -> Target.regular -> t
