(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This module contains Opentelemetry metrics for the core engine. Please review
   Ometrics.ml before adding more metrics. *)
val meter_scan_inputs :
  invalid_rules:Rule_error.invalid_rule list ->
  valid_rules:Rule.t list ->
  targets:Target.t list ->
  errors:Core_error.t list ->
  skipped:Semgrep_output_v1_t.skipped_target list ->
  unit
(** [meter_scan_inputs] records various metrics about our scan inputs*)
