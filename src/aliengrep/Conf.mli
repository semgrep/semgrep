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
   Parsing and matching configuration for aliengrep
*)

type t = {
  (* Use case-insensitive matching. Rely on PCRE to do this well. *)
  caseless : bool;
  (* multiline = newlines are treated as ordinary whitespace *)
  multiline : bool;
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  brackets : (char * char) list;
}

(* TODO: document the difference in the defaults *)
val default_multiline_conf : t
val default_singleline_conf : t

(* Check the validity of the configuration.
   Raises an exception if the configuration is invalid. *)
val check : t -> unit
