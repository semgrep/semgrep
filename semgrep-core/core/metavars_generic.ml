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

(* note that the Ast_generic.any acts as the value of the metavar and also
 * as its concrete code "witness". You can get position information from it,
 * it is not Parse_info.Ab(stractPos) *)
type metavars_binding = (mvar, Ast_generic.any) Common.assoc

(* ex: $X, $FAIL, $VAR2, $_ 
 * Note that some languages such as PHP or Javascript allows '$' in identifier
 * names, so forcing metavariables to have uppercase letters at least allow
 * us to match specifically also identifiers in lower case (e.g., $foo will
 * only match the $foo identifiers in some concrete code; this is not a
 * metavariable).
 * 
*)
let metavar_regexp_string = 
  "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"

(* 
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * ast.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s = 
  s =~ metavar_regexp_string
