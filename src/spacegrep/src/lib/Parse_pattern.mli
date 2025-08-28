(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching delimiters (), [], or {} within the same line.
*)

type error = { loc : Loc.t; msg : string }

val of_lexbuf : ?is_doc:bool -> Lexing.lexbuf -> (Pattern_AST.t, error) result
(** NOTE: Errors can only be returned when ~is_doc:false. *)

val of_src : ?is_doc:bool -> Src_file.t -> (Pattern_AST.t, error) result
(** NOTE: Errors can only be returned when ~is_doc:false. *)
