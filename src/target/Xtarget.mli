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

type t = private {
  path : Target.path;
  analyzer : Analyzer.t;  (** The analyzer to use when scanning this target. *)
  lazy_content : string Lazy_safe.t;
  lazy_utf8_valid : bool Lazy_safe.t;
      (** Whether [lazy_content] is valid UTF-8, computed once when needed. *)
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) Lazy_safe.t;
      (** This is valid only for analyzer = Analyzer.L ..., not for LRegex|LGeneric *)
  prefilter : case_sensitive:bool -> string -> bool option;
      (** [prefilter ~case_sensitive needle] tells if [needle] occurs in the
          content, or [None] if unknown. See {!make}. *)
}
(** Private so that the fields derived from the content stay consistent with
    it. *)

val make :
  ?prefilter:(string -> case_sensitive:bool -> string -> bool option) ->
  path:Target.path ->
  analyzer:Analyzer.t ->
  lazy_content:string Lazy_safe.t ->
  lazy_ast_and_errors:(AST_generic.program * Tok.location list) Lazy_safe.t ->
  unit ->
  t
(** The [prefilter] field is [prefilter content], applied on first use. It
    defaults to [Fun.const None]. *)

val with_ast : t -> (AST_generic.program * Tok.location list) Lazy_safe.t -> t
(** [with_ast xtarget ast] replaces the AST, keeping the content. *)

val resolve :
  ?prefilter:(string -> case_sensitive:bool -> string -> bool option) ->
  (Lang.t -> Fpath.t -> AST_generic.program * Tok.location list) ->
  Target.t ->
  t
(** [resolve parser target] is the extended version of [target], comprising
    also the contents and parsed version thereof. See {!make} for
    [prefilter]. *)

(* Instead of a parser (like with resolve), pass a lazy AST. This allows for the
 * easy construction of Xtargets in contexts where the client has already parsed
 * the file in question. *)
val resolve_with_ast :
  ?prefilter:(string -> case_sensitive:bool -> string -> bool option) ->
  (AST_generic.program * Tok.location list) Lazy_safe.t ->
  Target.t ->
  t
