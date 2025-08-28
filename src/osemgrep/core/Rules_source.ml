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
type t =
  (* For --pattern/--lang/--replacement (also -e/-l/--replacement).
   * In theory we could even parse the string to get a XPattern.t
   * Analyzer.t is now an option to allow to use -e without -l in osemgrep
   *)
  | Pattern of string * Analyzer.t option * string option (* replacement *)
  (* --config. In theory we could even parse the string to get
   * some Rules_config.t list *)
  | Configs of Rules_config.config_string list
(* TODO? | ProjectUrl of Uri.t? or just use Configs for it? *)
[@@deriving show]
