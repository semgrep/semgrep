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
type conf = {
  merge_partial_results_dir : Fpath.t option;
  merge_partial_results_output : Fpath.t option;
  validate_partial_results_expected : Fpath.t option;
  validate_partial_results_actual : Fpath.t option;
  upload_partial_results : Fpath.t option;
  upload_partial_results_scan_id : int option;
}
[@@deriving show]

val hook_pro_read_and_merge_partial_scan_results :
  (input_dir:Fpath.t -> output_json:Fpath.t -> unit) option Hook.t

val hook_pro_read_and_validate_partial_scan_results :
  (expected:Fpath.t -> actual:Fpath.t -> bool) option Hook.t

val hook_pro_read_and_upload_partial_scan_results :
  (Auth.token -> scan_id:int -> partial_results:Fpath.t -> bool) option Hook.t

val maybe_merge_partial_scan_results_then_exit : conf -> unit
val maybe_validate_partial_scan_results_then_exit : conf -> unit
val maybe_upload_partial_scan_results_then_exit : Auth.token -> conf -> unit
