(* Iago Abal
 *
 * Copyright (C) 2026 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(** Run-time taint coverage stats.
 *
 * For each taint source/sink in a rule, tracks how many times it matched
 * and across how many distinct files. *)

(*****************************************************************************)
(* Per-file per-rule stats *)
(*****************************************************************************)

type file_rule_stats
(** Per-file per-rule stats: source/sink match counts for one rule on one file. *)

val record_taint_spec_matches :
  sources:Rule.taint_source list ->
  sinks:Rule.taint_sink list ->
  file_rule_stats
(** Build per-file per-file stats from raw spec-matches in [Match_taint_spec]. *)

val merge_file_rule_stats : file_rule_stats list -> file_rule_stats
(** Merge a list of per-file stats into one, summing all match counts.
    EXPERIMENT: Group taint rules *)

(*****************************************************************************)
(* Coverage stats (accumulated across files) *)
(*****************************************************************************)

type 'spec spec_stats = {
  spec : 'spec;
  mutable total_matches : int;
  mutable num_files : int;
}

type rule_stats = {
  source_stats : (Rule.taint_spec_id, Rule.taint_source spec_stats) Hashtbl.t;
  sink_stats : (Rule.taint_spec_id, Rule.taint_sink spec_stats) Hashtbl.t;
}
(** Per-rule stats accumulated across all files. *)

type t = (Rule_ID.t, rule_stats) Hashtbl.t
(** Coverage stats table. *)

val create : unit -> t

val add :
  t ->
  rule_or_group:[ `Rule of Rule_ID.t | `Group of Taint_rule_group.t ] ->
  file_rule_stats ->
  unit
(** [add tbl ~rule_or_group file_rule_stats] accumulates [file_rule_stats] into
    the stats for the rule identified by [rule_or_group]. *)

val pretty : t -> string
(** Pretty-print coverage stats.

    For now we just print details for "relevan" rules that have unconstrained
    source matches AND sink matches, i.e. rules that may actually produce findings. *)

(*****************************************************************************)
(* Summary *)
(*****************************************************************************)

type summary = {
  lang : Lang.t;
  may_produce_findings : int;
      (** Non-zero unconstrained sources and non-zero sinks.

        We do not check if the specific sources we match are enough to in fact
        enable the specific sinks that we match. We could, but what we do now
        is simple and seems to be good enough. *)
  somewhat_relevant : int;
      (** Non-zero sources and non-zero sinks, but all sources have
          [source_requires] set (i.e. no unconstrained sources) so
          these rules still cannot produce findings. *)
  sources_but_no_sinks : int;  (** Non-zero sources but zero sinks. *)
  sinks_but_no_sources : int;  (** Non-zero sinks but zero sources. *)
  no_sources_no_sinks : int;  (** Zero sources and zero sinks. *)
}

val summary : lang:Lang.t -> t -> summary
val pretty_summary : summary -> string

val record_summary_in_span : summary -> Telemetry.scope -> unit
(** Attach summary counts as span attributes. *)
