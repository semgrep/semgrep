(*s: semgrep/core/match_result.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Match_result.t]] *)
type t = {
  rule: Rule.t;
  file: Common.filename;
  code: Ast_generic.any;
  env: Metavars_generic.metavars_binding;
}
(*e: type [[Match_result.t]] *)
(*e: semgrep/core/match_result.ml *)
