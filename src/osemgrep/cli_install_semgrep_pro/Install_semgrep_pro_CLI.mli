(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   'semgrep install-semgrep-pro' command-line parsing.
*)

(*
   The result of parsing a 'semgrep install-semgrep-pro' command.
*)

type conf = { common : CLI_common.conf; custom_binary : string option }
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-install-semgrep-pro"; <args> |]

   Turn argv into a conf structure.
*)
val parse_argv : string array -> conf
