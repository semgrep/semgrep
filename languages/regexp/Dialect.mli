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
(*
   Some predefined dialects. They can be used as a base for further dialects
   using the 'with' syntax, e.g.

     open Regexp.Conf
     let perl_xx = {
       Pcre2_.Dialect.perl with
       ignore_whitespace = true;
       ignore_whitespace_in_char_classes = true;
     }
*)

(*
   The list of regexp dialects with preset defaults.

   In doubt, use 'Pcre' since the PCRE library tries to accommodate
   the syntactic constructs of all the dialects.
*)
type t =
  | Go
  | Java
  | Javascript
  | PCRE
  | PCRE_extended (* ignore whitespace *)
  | Perl
  | Perl_x (* /x: ignore whitespace like PCRE_extended *)
  | Perl_xx (* /xx: ignore also whitespace in character classes *)
  | Python

(* Convert between lowercase/underscore representation and OCaml
   e.g. "pcre" for PCRE and "perl_xx" for Perl_xx. *)
val to_string : t -> string
val of_string : string -> t option
val conf : t -> Conf.t

(* The default is the same as PCRE with the default options. *)
val default_conf : Conf.t
