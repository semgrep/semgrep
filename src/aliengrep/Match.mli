(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Match a compiled pattern against a target string.
*)

(*
   Position and length of a matched substring, in bytes.
   To recover line/column information, see how it's done
   in Xpattern_match_regexp.ml.
*)
type loc = {
  start : int;
  length : int;
  (* The matched data. This is redundant but convenient for testing. *)
  substring : string;
}
[@@deriving show]

type match_ = {
  match_loc : loc;
  captures : (Pat_compile.metavariable * loc) list;
}
[@@deriving show]

(* For debugging *)
val show_matches : match_ list -> string

(* Search for matches in a target string. *)
val search : Pat_compile.t -> string -> match_ list
