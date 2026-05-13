(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** Semgrep-flavored OTLP collector endpoints and the alias map that lets a
    user write [semgrep-prod] / [semgrep-dev] / [semgrep-local] on the command
    line instead of a full URL. *)

val default_trace_endpoint : Uri.t
(** Production OTLP collector. Resolved from the [semgrep-prod] alias and
    used as the implicit default when [--trace] is set without an explicit
    endpoint. *)

val resolve : string -> Uri.t
(** [resolve s] expands the string aliases [semgrep-prod], [semgrep-dev], and
    [semgrep-local] to their corresponding default endpoint. Anything else is
    treated as a literal URL and parsed via [Uri.of_string]. *)
