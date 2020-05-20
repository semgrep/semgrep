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

(* mostly a copy-paste of metavars_fuzzy.ml and metavars_js.ml *)

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string AST_generic.wrap'?
 *)
(*s: type [[Metavars_generic.mvar]] *)
type mvar = string 
(*e: type [[Metavars_generic.mvar]] *)

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
 * 
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
  s =~ metavar_regexp_string
(*e: function [[Metavars_generic.is_metavar_name]] *)
(*e: semgrep/core/Metavars_generic.ml *)
