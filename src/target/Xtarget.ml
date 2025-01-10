(* Yoann Padioleau, Cooper Pierce
 *
 * Copyright (c) 2024, Semgrep Inc.
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

(* See the mli for usage documentation *)

type t = {
  path : Target.path;
  analyzer : Analyzer.t;
  lazy_content : string lazy_t;
  lazy_ast_and_errors : (AST_generic.program * Tok.location list) lazy_t;
}

let parse_file parser (analyzer : Analyzer.t) path =
  let lang =
    (* Possibly better to determine this sooner/change how lazy_ast_and_errors
       works for regex or other non-parsing analyzers *)
    match analyzer with
    | L (lang, []) -> lang
    | L (_lang, _ :: _) ->
        failwith
          "analyzer from the language field in -target should be unique (this \
           shouldn't happen FIXME)"
    | _ ->
        (* alt: could return an empty program, but better to be defensive *)
        failwith "requesting generic AST for an unspecified target language"
  in
  parser lang path

let resolve_with_ast ast (target : Target.regular) : t =
  {
    path = target.path;
    analyzer = target.analyzer;
    lazy_content = lazy (UFile.read_file target.path.internal_path_to_content);
    lazy_ast_and_errors = ast;
  }

let resolve parser (target : Target.regular) : t =
  let ast =
    lazy
      (parse_file parser target.analyzer target.path.internal_path_to_content)
  in
  resolve_with_ast ast target
