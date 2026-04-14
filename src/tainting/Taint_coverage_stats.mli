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

   For each taint source/sink in a rule, tracks how many times it matched
   and across how many distinct files. *)

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

val add : t -> rule:Rule_ID.t -> file_rule_stats -> unit
(** [add tbl ~rule file_rule_stats] accumulates [file_rule_stats] into
    the stats for the rule identified by [rule]. *)

val merge : src:t -> dst:t -> unit
(** [merge ~src ~dst] folds all entries from [src] into [dst].
    For rules present in both, source and sink match counts and file counts
    are summed. *)

val pretty : t -> string
(** Pretty-print coverage stats.

    Only prints details for "applicable" rules (see [rule_is_applicable]). *)

val rule_is_applicable : t -> Rule_ID.t -> bool
(** [rule_is_applicable stats rule_id] returns [true] if the rule has
    non-zero unconstrained sources and non-zero sinks across all analyzed files,
    i.e. it may produce taint findings.

    Returns [true] for rules that have no entry in [stats], to be safe, like for
    non-taint rules. *)

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
