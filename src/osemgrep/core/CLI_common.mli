(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Shared flags across the different Semgrep commands and utilities to help
   with command-line parsing and handling (relies on the cmdliner library)

   The o_ below stands for option (as in command-line argument option).
*)

type conf = {
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  simple_profiling : bool;
  profile : bool;
  (* mix of --experimental, --legacy, --develop *)
  maturity : Maturity.t;
  x_parmap : bool;
  (* Telemetry is `semgrep lsp`-specific for now. *)
  telemetry : Telemetry.config option;
}
[@@deriving show]

val experimental_section_title : string
(** Use [Arg.info ~docs:CLI_common.experimental_section_title] to document
    an option as experimental. *)

(* stuff to add after an option that is available only in semgrep-pro *)
val blurb_pro : string

(* handles logging arguments (--quiet/--verbose/--debug) *)
val o_logging : Logs.level option Cmdliner.Term.t

(* small wrapper around Logs_helper.setup_logging and Logging_helpers.setup *)
val with_logging :
  color:Console.highlight_setting ->
  level:Logs.level option ->
  (unit -> 'a) ->
  'a

(* for --simple-profiling *)
val o_simple_profiling : bool Cmdliner.Term.t
val o_profile : bool Cmdliner.Term.t

(* gather all the common flags under one term *)
val o_common : conf Cmdliner.Term.t
val help_page_bottom : Cmdliner.Manpage.block list

(* small wrapper around Cmdliner.Cmd.eval_value *)
val eval_value : argv:string array -> 'a Cmdliner.Cmd.t -> 'a

val exits : Cmdliner.Cmd.Exit.info list
(** All the exit codes to be passed as
    [Cmd.info ~exits:CLI_common.exits] so that they appear in [--help].
*)
