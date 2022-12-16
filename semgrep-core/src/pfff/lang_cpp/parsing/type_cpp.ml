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

open Ast_cpp

module Ast = Ast_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let is_function_type x =
  match Ast.unwrap_typeC x with
  | TFunction _ -> true
  | _ -> false

let rec is_method_type x =
  match Ast.unwrap_typeC x with
  | TPointer (_, y, _) ->
      is_method_type y
  | ParenType paren_ft ->
      is_method_type (Ast.unparen paren_ft)
  | TFunction _ ->
      true
  | _ -> false
