(** eXtended target.

   This type is mostly used in the engine to pass around extra information
   (e.g., contents, the AST) associated with each {{!Target.regular}target}.

   See also {!Input_to_core_t.target}, which is what is passed to
   [semgrep-core] via [-target].
 *)

type t = {
  path : Target.path;
  xlang : Xlang.t;  (** The analyzer to use when scanning this target. *)
  lazy_content : string lazy_t;
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
      (** This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
}

val resolve :
  (Language.t -> Fpath.t -> AST_generic.program * Tok.location list) ->
  Target.regular ->
  t
(** [resolve parser target] is the extended version of [target], comprising
    also the contents and parsed version thereof. *)
