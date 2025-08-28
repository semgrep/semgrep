(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
  'semgrep validate' command-line parsing
*)

(* The result of parsing a 'semgrep validate' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep --validate <dir>'
 * into the new 'semgrep validate <dir>'
 *)
type conf = {
  rules_source : Rules_source.t;
  pro : bool;
  core_runner_conf : Core_runner.conf;
  common : CLI_common.conf;
}
[@@deriving show]

val parse_argv : string array -> conf
