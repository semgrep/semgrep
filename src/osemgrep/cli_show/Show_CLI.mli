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
   'semgrep show' command-line parsing.
*)

(*
   The result of parsing a 'semgrep show' command.
   This is also used in Scan_CLI.ml to transform legacy
   commands such as 'semgrep scan --show-supported-languages' into the
   new 'semgrep show supported-languages'
*)
type conf = {
  common : CLI_common.conf;
  (* mix of --dump-ast/--dump-rule/... *)
  show_kind : show_kind;
  json : bool;
}

and show_kind =
  | Version
  | SupportedLanguages
  | Identity
  | Deployment
  | ProjectRoot of { scan_root : Fpath.t }
  | Resources
  (* dumpers *)
  | DumpPattern of string * Lang.t
  | DumpCST of Fpath.t * Lang.t
  | DumpAST of Fpath.t * Lang.t
  | DumpConfig of Rules_config.config_string
  | DumpTargets of
      Scanning_root.t * Find_targets.conf * Rules_config.config_string option
  | DumpEnginePath of bool (* pro = true *)
  | DumpCommandForCore
  | FilePrefilterOfRules of Fpath.t
  (* pro-only commands *)
  | Debug of debug_settings  (** Open an interactive debugging view. *)
  | DumpLockfile of Fpath.t (* lockfile *) * Fpath.t option (* manifest *)
  | ProjectPrefilterOfRules of Fpath.t

and debug_settings = {
  output_dir : Fpath.t;
      (** Directory to save the output to. If not specified on command line,
          defaults to a temporary directory. *)
  targeting_conf : Find_targets.conf;
      (** Configuration for finding target files to debug. Built from the root
          argument with default settings. *)
  rules_source : Rules_source.t;
      (** Configuration for rules to use when debugging *)
}
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-show"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : < Cap.tmp ; .. > -> string array -> conf
