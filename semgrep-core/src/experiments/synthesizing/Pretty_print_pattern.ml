(* Emma Jin
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
open Common
open AST_generic
open Pretty_print_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This used to be in Pretty_print_AST.ml
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pattern_to_string lang any =
  (*let mvars = [] in *)
  match any with
  | E e -> expr_to_string lang (*mvars*) e
  | S s -> stmt_to_string lang (*mvars*) s
  | Ss stmts ->
      List.map (stmt_to_string lang (*mvars*)) stmts |> String.concat "\n"
  | Args args -> arguments_to_string (*{ lang; mvars }*) lang args
  | _ ->
      pr2 (AST_generic.show_any any);
      failwith "todo: only expression pattern can be pretty printed right now"
