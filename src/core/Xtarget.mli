(* Yoann Padioleau, Cooper Pierce
 *
 * Copyright (c) Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

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
