(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type t = {
  path : Fpath.t;
  (* 0-based byte index, inclusive *)
  start : int;
  (* 0-based byte index, exclusive *)
  end_ : int;
  replacement_text : string;
}
[@@deriving show]

type edit_application_result =
  | Success of string
  | Overlap of {
      partial_result : string;
      (* nonempty *)
      conflicting_edits : t list;
    }

(* Apply a list of edits, modifying the files in place. If dryrun, do everything
 * but write to the files.
 *
 * Returns the list of modified files and the list of edits that were not
 * applied because they overlapped with others. *)
val apply_edits : dryrun:bool -> t list -> Fpath.t list * t list

(* Applies the edits to the given text and returns the result. Pure function. *)
val apply_edits_to_text : Fpath.t -> string -> t list -> edit_application_result

(* Applies the edit to the given text and returns the resulting string. Pure
 * function. *)
val apply_edit_to_text : string -> t -> string
