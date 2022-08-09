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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Explanations of how something was matched.
 * This is useful for a step-by-step debugger of the matching process.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  op : matching_operation;
  children : t list;
  (* resulting ranges *)
  matches : Pattern_match.t list;
  (* TODO: should be a range loc in the rule file *)
  pos : Rule.tok;
}

(* TODO:
 * - tainting source/sink/sanitizer
 * - subpattern EllipsisAndStmt, ClassHeaderAndElems
 * - Where filters (metavar-comparison, etc)
 *)
and matching_operation =
  | OpAnd
  | OpOr
  (*  | OpNot *)
  | OpXPattern
[@@deriving show]
