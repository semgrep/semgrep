(* 
 *
 * Copyright (C) 2013 Facebook
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

open Ast_php
module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Yet another code pretty printer ...
 * 
 * related:
 *  - lang_php/parsing/unparse_php.ml
 *  - lang_php/pretty/
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let pp buf s =
  Buffer.add_string buf s


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec string_of_program ast =
  let buf = Buffer.create 100 in
  List.iter (stmt buf) ast;
  Buffer.contents buf

and stmt buf st =
  match st with
  | FuncDef def ->
    pp buf (spf "function %s(...) {\n" (Ast.str_of_ident def.f_name));
    List.iter (stmt buf) def.f_body;
    pp buf "}\n"
              
  | Expr (e, _) ->
    expr buf e

  | _ -> raise Todo

and expr buf e =
  match e with
  | Call _ -> pp buf "call;\n"
  | _ -> raise Todo
