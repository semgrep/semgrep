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
   Type of the AST for an aliengrep pattern.

   This will be converted into a PCRE regexp for matching a target file.
*)

type t = node list

(*
   The allowed characters for Bracket and for Word depend on the configuration
   of the parser as provided by Conf.t.
*)
and node =
  | Ellipsis
  | Long_ellipsis
  | Metavar of string (* identifier "FOO" only without "$" *)
  | Metavar_ellipsis of string (* same *)
  | Long_metavar_ellipsis of string (* same *)
  | Bracket of char * t * char
  | Word of string (* a word may not be adjacent to another word *)
  | Newline
  | Other of string
[@@deriving show]

(*
   Check the validity of an AST, raising an exception in case of an error.
   Errors include a variable name that occurs multiple times but under
   different types such as '$FOO' and '$...FOO'.
*)
val check : t -> unit
