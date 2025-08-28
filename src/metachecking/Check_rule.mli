(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val check : Rule.t -> Core_error.t list

(* to test -check_rules *)
val run_checks :
  < Core_scan.caps ; Cap.readdir ; .. > ->
  Fpath.t (* metachecks *) ->
  Fpath.t list (* rules *) ->
  Core_error.t list

(* -check_rules *)
val check_files :
  < Cap.stdout ; Core_scan.caps ; Cap.readdir ; .. > ->
  Core_scan_config.output_format ->
  Fpath.t list ->
  unit

(* -stat_rules *)
val stat_files : < Cap.stdout ; Cap.readdir ; .. > -> Fpath.t list -> unit
