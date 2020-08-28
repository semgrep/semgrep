(*s: scope_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2011 Facebook
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
(*e: Facebook copyright *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * It would be more convenient to move this file elsewhere like in analyse_php/
 * but we want our AST to contain scope annotations so it's convenient to 
 * have the type definition of PHP scope here in parsing_php/. 
 * See also type_php.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type phpscope *)
type phpscope = Scope_code.t

 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: type phpscope *)

let s_of_phpscope = Scope_code.string_of_scope

(*e: scope_php.ml *)
