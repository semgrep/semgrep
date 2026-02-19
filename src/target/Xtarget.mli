(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** eXtended target.

   This type is mostly used in the engine to pass around extra information
   (e.g., contents, the AST) associated with each {{!Target.regular}target}.
 *)

type t = {
  path : Target.path;
  analyzer : Analyzer.t;  (** The analyzer to use when scanning this target. *)
  lazy_content : string Lazy_safe.t;
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) Lazy_safe.t;
      (** This is valid only for analyzer = Analyzer.L ..., not for LRegex|LGeneric *)
}

val resolve :
  (Lang.t -> Fpath.t -> AST_generic.program * Tok.location list) ->
  Target.t ->
  t
(** [resolve parser target] is the extended version of [target], comprising
    also the contents and parsed version thereof. *)

(* Instead of a parser (like with resolve), pass a lazy AST. This allows for the
 * easy construction of Xtargets in contexts where the client has already parsed
 * the file in question. *)
val resolve_with_ast :
  (AST_generic.program * Tok.location list) Lazy_safe.t -> Target.t -> t
