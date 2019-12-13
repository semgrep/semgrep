(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* mostly a copy-paste of metavars_fuzzy.ml and metavars_js.ml *)

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string Ast_generic.wrap'?
 *)
type mvar = string 

type metavars_binding = (mvar, Ast_generic.any) Common.assoc

(* ex: $X, $FAIL *)
let metavar_regexp_string = 
  "^\\(\\$[A-Z]+\\)$"

(* ex: $x *)
let metavar_variable_regexp_string = 
  "^\\(\\$[a-z]\\)$"

(* 
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * ast.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s = 
  s =~ metavar_regexp_string

let is_metavar_variable_name s = 
  s =~ metavar_variable_regexp_string
