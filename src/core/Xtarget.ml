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
   (e.g., contents, the AST) associated with each {{!Target.code}target}.

   See also {!Input_to_core_t.target}, which is what is passed to
   [semgrep-core] via [-target].
 *)

type t = {
  path : Target.target_path;
  xlang : Xlang.t;  (** The analyzer to use when scanning this target. *)
  lazy_content : string lazy_t;
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
      (** This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
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

let resolve parser (target : Target.code) : t =
  {
    path = target.path;
    xlang = target.analyzer;
    lazy_content = lazy (UFile.read_file target.path.internal_path_to_content);
    lazy_ast_and_errors =
      parse_file parser target.analyzer target.path.internal_path_to_content;
  }
