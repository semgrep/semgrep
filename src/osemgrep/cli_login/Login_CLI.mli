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
   'semgrep login' command-line parsing.
*)

(*
   The result of parsing a 'semgrep login' command.
*)
type conf = {
  common : CLI_common.conf;
  (* Initialize the auth exchange with a temporary shared secret *)
  one_time_seed : string;
}
[@@deriving show]

(*
   Usage: parse_argv cmd_info [| "semgrep-login"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
