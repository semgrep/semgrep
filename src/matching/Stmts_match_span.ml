(* Martin Jambon
 *
 * Copyright (C) 2022 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = AST_generic.stmt list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let extract_tokens stmts =
  AST_generic_helpers.ii_of_any (Ss stmts) |> List.filter Tok.is_origintok

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Add a statement to the set of statements matched by the current pattern,
   for the sake of eventually determining the match location. *)
let extend stmt span = stmt :: span
let location stmts = AST_generic_helpers.range_of_any_opt (AST_generic.Ss stmts)
let list_original_tokens stmts = extract_tokens stmts
