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
module Out = Semgrep_output_v1_t

val syntactic_id : Out.cli_match -> string
(** A unique key designed with notification user experience in mind.
    Results in fewer unique findings than core_unique_key.

    This uses the Murmur3 128 hash, and is used e.g. in Gitlab_sast and
    Gitlab_secrets output. *)

val match_based_id_partial :
  (module Digestif.S) ->
  Rule.t ->
  Rule_ID.t ->
  Out.metavars option ->
  string ->
  string
(** The fingerprint used to uniquely identify a match. Since this is used by the
    backend, it is crucial to have identical output as in pysemgrep. *)

(* for unit testing *)
val match_formula_interpolated_str :
  Rule.t -> Semgrep_output_v1_t.metavars option -> string

val rule_checksum_str : Rule.t -> string
(** Compute a checksum for a rule based on its content. *)
