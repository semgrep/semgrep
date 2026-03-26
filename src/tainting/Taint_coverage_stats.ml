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

(* THINK: Can/should this be merged with Core_quick_profiling stats ?? *)

(** Run-time taint coverage stats.
 *
 * For each taint source/sink in a rule, tracks how many times it matched
 * and across how many distinct files. *)

(*****************************************************************************)
(* Per-file per-rule stats *)
(*****************************************************************************)

module SourceMap = Map.Make (struct
  type t = Rule.taint_source

  let compare s1 s2 = String.compare s1.Rule.source_id s2.Rule.source_id
end)

module SinkMap = Map.Make (struct
  type t = Rule.taint_sink

  let compare s1 s2 = String.compare s1.Rule.sink_id s2.Rule.sink_id
end)

type file_rule_stats = {
  file_source_stats : int SourceMap.t;
  file_sink_stats : int SinkMap.t;
}
(** Per-file per-rule stats: source/sink match counts for one rule on one file. *)

let record_taint_spec_matches ~sources ~sinks =
  let incr_count = function
    | None -> Some 1
    | Some n -> Some (n + 1)
  in
  let file_source_stats =
    sources
    |> List.fold_left
         (fun stats (source : Rule.taint_source) ->
           SourceMap.update source incr_count stats)
         SourceMap.empty
  in
  let file_sink_stats =
    sinks
    |> List.fold_left
         (fun stats (sink : Rule.taint_sink) ->
           SinkMap.update sink incr_count stats)
         SinkMap.empty
  in
  { file_source_stats; file_sink_stats }

(*****************************************************************************)
(* Rule stats (accumulated across files) *)
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

let create_rule_stats () =
  { source_stats = Hashtbl.create 1; sink_stats = Hashtbl.create 1 }

let add_file_stats_into stats file_stats =
  file_stats.file_source_stats
  |> SourceMap.iter (fun (source : Rule.taint_source) total_matches ->
         match Hashtbl.find_opt stats.source_stats source.source_id with
         | None ->
             Hashtbl.add stats.source_stats source.source_id
               { spec = source; total_matches; num_files = 1 }
         | Some ss ->
             ss.total_matches <- ss.total_matches + total_matches;
             ss.num_files <- ss.num_files + 1);
  file_stats.file_sink_stats
  |> SinkMap.iter (fun (sink : Rule.taint_sink) total_matches ->
         match Hashtbl.find_opt stats.sink_stats sink.sink_id with
         | None ->
             Hashtbl.add stats.sink_stats sink.sink_id
               { spec = sink; total_matches; num_files = 1 }
         | Some ss ->
             ss.total_matches <- ss.total_matches + total_matches;
             ss.num_files <- ss.num_files + 1)

(*****************************************************************************)
(* Coverage stats *)
(*****************************************************************************)

type t = (Rule_ID.t, rule_stats) Hashtbl.t
(** Stats table mapping rule IDs to per-rule stats across all files. *)

let create () : t = Hashtbl.create 100

let add (stats : t) ~rule (file_rule_stats : file_rule_stats) =
  let rule_stats =
    match Hashtbl.find_opt stats rule with
    | None ->
        let rule_stats = create_rule_stats () in
        Hashtbl.add stats rule rule_stats;
        rule_stats
    | Some rule_stats -> rule_stats
  in
  add_file_stats_into rule_stats file_rule_stats

(*****************************************************************************)
(* Rule applicability *)
(*****************************************************************************)

(* A rule is relevant if it has a chance to produce findings, meaning:

  - The rule matches *at least* one _unconstrained_ source.
  - The rule matches *at least* one sink.

  With taint labels, a source may have constraints (see `requires:` in the rule
  language) that make it only apply conditionally, if another source has matched
  before. For example, a source with `requires: INPUT` is only enabled if there
  is another source producing the label `INPUT`. So we need at least one source
  without constraints to match in order to produce a finding.

  In fact, not all these rules will be truly applicable, because we would also
  need to check that the sinks that match can be activated by the sources that
  match. But this predicate is good enough for now.
  *)

let is_applicable (r : rule_stats) =
  let has_unconstrained_sources =
    r.source_stats |> Hashtbl.to_seq
    |> Seq.exists (fun (_id, ss) -> ss.spec.Rule.source_requires = None)
  in
  let has_any_sinks = Hashtbl.length r.sink_stats > 0 in
  has_unconstrained_sources && has_any_sinks

let rule_is_applicable (tbl : t) (rule_id : Rule_ID.t) =
  match Hashtbl.find_opt tbl rule_id with
  | None -> true (* in case of doubt, keep it *)
  | Some r -> is_applicable r

(*****************************************************************************)
(* Pretty printing *)
(*****************************************************************************)

let print_spec_stats buf ~kind (ss_list : (string * _ spec_stats) list) =
  List.iter
    (fun (id, ss) ->
      Buffer.add_string buf
        (Printf.sprintf "  %s %s: %d matches in %d files\n" kind id
           ss.total_matches ss.num_files))
    ss_list

let pretty (stats : t) =
  (* Only print rules with unconstrained sources AND sinks, i.e. rules that
     are applicable (see [is_applicable]). *)
  let sort_by_id xs =
    List.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) xs
  in
  let num_not_applicable = ref 0 in
  let buf = Buffer.create 512 in
  stats |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (id1, _) (id2, _) -> Rule_ID.compare id1 id2)
  |> List.iter (fun (rule_id, r) ->
         if not (is_applicable r) then incr num_not_applicable
         else begin
           let sources =
             r.source_stats |> Hashtbl.to_seq
             |> Seq.filter (fun (_id, ss) ->
                    (* Necessary sources (no 'requires:') *)
                    ss.spec.Rule.source_requires = None)
             |> List.of_seq |> sort_by_id
           in
           let sinks =
             r.sink_stats |> Hashtbl.to_seq |> List.of_seq |> sort_by_id
           in
           Buffer.add_string buf
             (Printf.sprintf "Rule %s:\n" (Rule_ID.to_string rule_id));
           print_spec_stats buf ~kind:"source" sources;
           print_spec_stats buf ~kind:"sink" sinks
         end);
  if !num_not_applicable > 0 then
    Buffer.add_string buf
      (Printf.sprintf "\n!!! %d rules are not applicable\n" !num_not_applicable);
  Buffer.contents buf

(*****************************************************************************)
(* Summary *)
(*****************************************************************************)

type summary = {
  lang : Lang.t;
  may_produce_findings : int;
      (** Non-zero unconstrained sources and non-zero sinks. *)
  somewhat_relevant : int;
      (** Non-zero sources and non-zero sinks, but all sources have
          a `requires:` so they are not sufficient to generate findings. *)
  sources_but_no_sinks : int;  (** Non-zero sources but zero sinks. *)
  sinks_but_no_sources : int;  (** Non-zero sinks but zero sources. *)
  no_sources_no_sinks : int;  (** Zero sources and zero sinks. *)
}

let summary ~lang (tbl : t) =
  let may_produce_findings = ref 0 in
  let somewhat_relevant = ref 0 in
  let sources_but_no_sinks = ref 0 in
  let sinks_but_no_sources = ref 0 in
  let no_sources_no_sinks = ref 0 in
  tbl
  |> Hashtbl.iter (fun _rule_id r ->
         let has_any_sources = Hashtbl.length r.source_stats > 0 in
         let has_any_sinks = Hashtbl.length r.sink_stats > 0 in
         if is_applicable r then incr may_produce_findings
         else
           match (has_any_sources, has_any_sinks) with
           | true, true -> incr somewhat_relevant
           | true, false -> incr sources_but_no_sinks
           | false, true -> incr sinks_but_no_sources
           | false, false -> incr no_sources_no_sinks);
  {
    lang;
    may_produce_findings = !may_produce_findings;
    somewhat_relevant = !somewhat_relevant;
    sources_but_no_sinks = !sources_but_no_sinks;
    sinks_but_no_sources = !sinks_but_no_sources;
    no_sources_no_sinks = !no_sources_no_sinks;
  }

let pretty_summary s =
  Printf.sprintf
    "Taint rule coverage summary for %s:\n\
    \  may produce findings:\t %d\n\
    \  somewhat relevant:\t %d\n\
    \  sources but no sinks:\t %d\n\
    \  sinks but no sources:\t %d\n\
    \  no sources no sinks:\t %d\n"
    (Lang.to_string s.lang) s.may_produce_findings s.somewhat_relevant
    s.sources_but_no_sinks s.sinks_but_no_sources s.no_sources_no_sinks

let record_summary_in_span summary span =
  Tracing.add_data_to_span span
    [
      ("lang", `String (Lang.to_string summary.lang));
      ( "taint.interfile.coverage.may_produce_findings",
        `Int summary.may_produce_findings );
      ( "taint.interfile.coverage.somewhat_relevant",
        `Int summary.somewhat_relevant );
      ( "taint.interfile.coverage.sources_but_no_sinks",
        `Int summary.sources_but_no_sinks );
      ( "taint.interfile.coverage.sinks_but_no_sources",
        `Int summary.sinks_but_no_sources );
      ( "taint.interfile.coverage.no_sources_no_sinks",
        `Int summary.no_sources_no_sinks );
    ]
