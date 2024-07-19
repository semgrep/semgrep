(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A semgrep pattern.
 *
 * A pattern is represented essentially as an AST_generic.any
 * where special constructs are now allowed (e.g., Ellipsis),
 * where certain identifiers are metavariables (e.g., $X), or
 * where certain strings are ellipsis or regular expressions
 * (e.g., "=~/foo/").
 * TODO: deprecate the regexps strings
 *
 * See also Mvar.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* right now Expr/Stmt/Stmts/Types/Patterns/Partial and probably
 * more are supported *)
type t = AST_generic.any [@@deriving show, eq]

(* TODO: deprecate this *)
let regexp_regexp_string = "^=~/\\(.*\\)/\\([mi]?\\)$"

(* TODO: deprecate this *)
let is_regexp_string s = s =~ regexp_regexp_string

let is_special_string_literal str =
  str = "..." || Mvar.is_metavar_name str
  || Mvar.is_metavar_ellipsis str
  (* TODO: deprecate this *)
  || is_regexp_string str

let is_js lang =
  match lang with
  | Some x -> Lang.is_js x
  | None -> true

(* This is used in Analyze_pattern.ml to skip
 * semgrep special identifiers.
 * new TODO: instead of relying on this list of exceptions, set 'hidden' flag
 * in the 'id_info'.
 *)
let is_special_identifier ?lang str =
  Mvar.is_metavar_name str
  (* emma: a hack because my regexp skills are not great *)
  || (String.length str > 4 && Str.first_chars str 4 = "$...")
  (* in JS field names can be regexps *)
  || (is_js lang && is_regexp_string str)
  (* ugly hack that we then need to handle also here *)
  || str = AST_generic.special_multivardef_pattern
  (* ugly: because ast_js_build introduce some extra "!default" ids *)
  || (is_js lang && str = Ast_js.default_entity)
  (* parser_java.mly inserts some implicit this *)
  || (lang =*= Some Lang.Java && str = "this")
  || (* TODO: PHP converts some Eval in __builtin *)
  (lang =*= Some Lang.Php && str =~ "__builtin__*")
