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

val regexp_matcher :
  ?base_offset:int ->
  ?utf8_valid:bool ->
  (* str *) string ->
  Fpath.t ->
  Pcre2_.t ->
  ((Tok.location * Tok.location) * (string * Metavariable.mvalue) list) list
(** [utf8_valid] must describe all of [big_str], when supplied. A known invalid
    UTF-8 string has no matches because PCRE2 patterns are compiled in UTF
    mode. *)

val matches_of_regexs :
  utf8_valid:bool Lazy_safe.t ->
  (Pcre2_.t * Xpattern.pattern_id * string) list ->
  string Lazy_safe.t ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result
(** [utf8_valid] must describe the contents of [lazy_content]. *)
