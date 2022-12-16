(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * An Abstract Syntax Tree for Skip.
 *
 * I do an AST as in ast_java.ml because I need less a CST
 * now that we use the fuzzy approach for sgrep and spatch.
 * I just keep the Parse_info.info for the identifiers and literals.
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.t

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * tok

(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
type name = string wrap

(* lower and uppernames aliases, just for clarity *)
and lname = name
and uname = name

(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Patterns *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Functions *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Type declarations *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Classes *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Modules *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Toplevel elements *)
(* ------------------------------------------------------------------------- *)

type program = unit

(* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | Program of program
  | Info of tok

(* with tarzan *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let str_of_name (s,_) = s
let info_of_name (_,info) = info
