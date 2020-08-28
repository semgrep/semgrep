(* Yoann Padioleau
 * 
 * Copyright (C) 2014 Facebook
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
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The AST types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type info = Parse_info.t
type tok = info

type 'a wrap = 'a * tok
type 'a paren   = tok * 'a * tok

(* ------------------------------------------------------------------------- *)
(* Sexp *)
(* ------------------------------------------------------------------------- *)

type sexp =
  | Sexp of sexp list paren (* or backet actually *)
  | Atom of atom
  | Special of special wrap * sexp

and special =
  | Quote
  | BackQuote
  | Comma
  | At

and atom = 
  | Number of string wrap
  | String of string wrap
  | Id of string wrap

(* with tarzan *)

type program = sexp list

