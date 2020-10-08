(*s: semgrep/core/Metavars_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: pad/r2c copyright *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Mostly a copy-paste of metavars_fuzzy.ml and metavars_js.ml *)

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string AST_generic.wrap'?
 *)
(*s: type [[Metavars_generic.mvar]] *)
type mvar = string
(*e: type [[Metavars_generic.mvar]] *)

(* See the comment on Semgrep_generic.match_sts_sts for more information.
 * This is ugly, and we should probably use a variant for mvar
 * to differentiate semgrep metavariables from this special metavariable
 * used internally to help return correct statements ranges, but I'm
 * lazy.
 *)
let matched_statements_special_mvar = "!STMT!"

(*s: type [[Metavars_generic.metavars_binding]] *)
(* note that the AST_generic.any acts as the value of the metavar and also
 * as its concrete code "witness". You can get position information from it,
 * it is not Parse_info.Ab(stractPos) *)
type metavars_binding = (mvar, AST_generic.any) Common.assoc
(*e: type [[Metavars_generic.metavars_binding]] *)

(*s: constant [[Metavars_generic.metavar_regexp_string]] *)
(* ex: $X, $FAIL, $VAR2, $_
 * Note that some languages such as PHP or Javascript allows '$' in identifier
 * names, so forcing metavariables to have uppercase letters at least allow
 * us to match specifically also identifiers in lower case (e.g., $foo will
 * only match the $foo identifiers in some concrete code; this is not a
 * metavariable).
 * We allow _ as a prefix to disable the unused-metavar check (we use
 * the same convention than OCaml).
 * However this conflicts with PHP superglobals, hence the special
 * cases below in is_metavar_name.
 *)
let metavar_regexp_string =
  "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"
(*e: constant [[Metavars_generic.metavar_regexp_string]] *)

(*s: function [[Metavars_generic.is_metavar_name]] *)
(*
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * AST.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s =
  match s with
  (* ugly: we should probably pass the language to is_metavar_name, but
   * that would require to thread it through lots of functions, so for
   * now we have this special case for PHP superglobals.
   * ref: https://www.php.net/manual/en/language.variables.superglobals.php
   *)
  | "$_SERVER" | "$_GET" | "$_POST" | "$_FILES"
  | "$_COOKIES" | "$_REQUEST" | "$_ENV"
  (* todo: there's also "$GLOBALS" but this may interface with existing rules*)
  ->
     false
  | _ ->
    s =~ metavar_regexp_string
(*e: function [[Metavars_generic.is_metavar_name]] *)
(*e: semgrep/core/Metavars_generic.ml *)
