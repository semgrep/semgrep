(* Dario Teixeira
 *
 * Copyright (C) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in the license.txt file.
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file contains the type definitions for a Cascading Style Sheet
 * file, aka CSS file.
 *
 * See also http://en.wikipedia.org/wiki/Cascading_Style_Sheets
 *
 * Most of the code in this file is copy pasted from Dario Teixera
 * css parser and preprocessor: https://github.com/darioteixeira/ccss
 * I've mainly removed the use of open variants and added location
 * information a la pfff.
 *
 * alternatives:
 * - css parser and preprocessor: http://forge.ocamlcore.org/projects/ccss/
 * - camlp4 and css https://github.com/samoht/cass
*)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* From http://en.wikipedia.org/wiki/Cascading_Style_Sheets#Syntax:
 * "A style sheet consists of a list of rules. Each rule or rule-set consists
 *  of one or more selectors and a declaration block. A declaration-block
 *  consists of a list of declarations in braces. Each declaration itself
 *  consists of a property, a colon (:), a value. If there are multiple
 *  declarations in a block, a semi-colon (;) must be inserted to
 *  separate each declaration"
*)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type info = Parse_info.t
and tok = info

(* ------------------------------------------------------------------------- *)
(* Selector *)
(* ------------------------------------------------------------------------- *)

type selector = simplesel * (combinator * simplesel) list

and simplesel =
  | Explicit of element * qualifier list
  | Generic of qualifier * qualifier list

and combinator = Descendant | GeneralSibling | AdjacentSibling | Child

and element =
  | Tag of string
  | Universal

and qualifier =
  | Id of string
  | Class of string
  | Attr of string * attr
  | PseudoClass of string
  | PseudoElement of string
  | SelFunc of string * functiont

and functiont =
  | Qualified of qualifier list
  | Nth of string

and attr =
  | AttrExists
  | AttrEquals of string
  | AttrIncludes of string
  | AttrDashmatch of string
  | AttrPrefix of string
  | AttrSuffix of string
  | AttrSubstring of string

(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Declaration *)
(* ------------------------------------------------------------------------- *)

type declaration = property * expression * important

and property = string
and important = bool

and expression = sentence list
and sentence = term list

and term =
  | Calc of calc
  | String of string
  | Ident of string
  | Uri of string
  | Hash of string
  | TermFunc of string * expression
  | Slash

and calc =
  | Varref of variable
  | Quantity of quantity
  (* ccss only *)
  | Sum of calc * calc
  | Sub of calc * calc
  | Mul of calc * calc
  | Div of calc * calc

and quantity = float * string option

and variable = string

(* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Rule *)
(* ------------------------------------------------------------------------- *)
type rule = selector list * declaration list
(* with tarzan *)

type stylesheet = rule list
(* with tarzan *)
