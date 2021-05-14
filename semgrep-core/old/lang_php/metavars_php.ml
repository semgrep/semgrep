(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
module Ast = Cst_php
module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* mostly copy of metavars_fuzzy.ml *)

(* todo? could want to remember the position in the pattern of the metavar
 * for error reporting ? so use a 'string Ast_php.wrap' ?
 *)
type mvar = string

type metavars_binding = (mvar, Cst_php.any) Common.assoc

(* bugfix: don't forget \\b, otherwise a string like FBredirect would
 * match such regexp (the starting F) even if it's not a metavar at all.
 *
 * examples: X, X1, X1_ILOVEPUPPIES
 *)
let metavar_regexp_string = "\\b\\([A-Z][0-9]?\\(_[A-Z0-9]*\\)?\\)\\b"

(* example: MANY_ARGS1 *)
let metavar_manyargs_regexp_string =
  "\\b\\(MANYARGS[0-9]?\\([_A-Z0-9]*\\)?\\)\\b"

(* examples: $X *)
let metavar_variable_regexp_string = "\\(\\$[A-Z]\\)\\b"

(*
 * Hacks abusing existing PHP constructs to encode extra constructions.
 * One day we will have a pattern_php_ast.ml that mimics mostly
 * ast_php.ml and extends it with sgrep special constructs.
 *)
let is_metavar_name s = s =~ metavar_regexp_string

let is_metavar_variable_name s = s =~ metavar_variable_regexp_string

let is_metavar_manyargs_name s = s =~ metavar_manyargs_regexp_string

(*****************************************************************************)
(* Checker *)
(*****************************************************************************)

let check_pattern any =
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kexpr =
          (fun (k, _) x ->
            match x with
            | Ast.IdVar (dname, _scope) ->
                let s = Ast.str_of_dname dname in
                if s =~ "V\\(_[A-Z]*\\)?" then
                  failwith
                    ( "Lvalue metavariables are deprecated, just use "
                    ^ "expression metavariables as in X->foo()" );
                k x
            | _ -> k x);
      }
  in
  visitor any;
  any
