(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
exception Error of string

(* this does not raise Error (but bailout with SCA_version.Other) *)
val parse : string -> SCA_version.t

(* used in Parse_rule.ml, may raise Error *)
val parse_constraints : string -> SCA_pattern.version_constraints
