(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
module AST = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Typing of the generic AST expressions.
 *
 * Note that right now this module is only used by Graph_code_AST.ml
 * during name resolving as we need to remember the type of entities
 * and expressions to be able to resolve field or method access.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: polymorphic types *)
type t =
  (* fully qualified name, as in graph_code, for L.lookup_dotted_ident_opt
   * less: use Graph_code.node instead?
   *)
  | N of AST.dotted_ident
  (* a few builtins *)
  | Builtin of string AST.wrap
  | List of t
  | Function of AST.parameters (* TODO? normalize also params? *) * t
  (* todos *)
  | Todo of AST.todo_kind
[@@deriving show]

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)
