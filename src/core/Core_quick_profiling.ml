(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc.
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

open Common

(* From all the "very slow" files/rules/etc, we report the "top N" ones.
  See 'Summary_stas.Key.very_slow_top_size'. *)
let very_slow_top_size = 10

(* NOTE(iago): The 'very_slow_threshold's are (of course) somewhat opinionated.
  As a rule thumb, for the average project, the thresholds should be above the
  mean time, and the very-slow-files count should be a small percentage of the
  overall count. *)

module Parsing_stats = Summary_stats.Make (struct
  type t = Fpath.t [@@deriving show]

  (* If parsing a file takes more than 0.3s, it's very slow. *)
  let very_slow_threshold = 0.3
  let very_slow_top_size = very_slow_top_size
end)

module Scanning_stats = Summary_stats.Make (struct
  type t = Fpath.t [@@deriving show]

  (* If scanning a file (with all rules) takes more than 1.5s, it's very slow. *)
  let very_slow_threshold = 1.5
  let very_slow_top_size = very_slow_top_size
end)

module Matching_stats = Summary_stats.Make (struct
  type t = Fpath.t * Rule_ID.t [@@deriving show]

  (* If matching a rule on a file takes more than 0.1s, it's very slow. *)
  let very_slow_threshold = 0.1
  let very_slow_top_size = very_slow_top_size
end)

module Tainting_stats = Summary_stats.Make (struct
  type t = Fpath.t * Pos.t * Rule_ID.t [@@deriving show]

  (* If running a taint rule (dataflow-only) on a definition takes more than 0.05s,
    it's very slow. *)
  let very_slow_threshold = 0.05
  let very_slow_top_size = very_slow_top_size
end)

module Prefiltering_stats = struct
  (* Internal count structure for accumulating stats during processing *)
  type t = {
    project_level_time : float;
    file_level_time : float;
    rules : int;
    rules_with_project_prefilters : int;
    rules_with_file_prefilters : int;
    rules_selected : int;
    rules_matched : int;
  }
  [@@deriving show]

  let zero : t =
    {
      project_level_time = 0.0;
      file_level_time = 0.0;
      rules = 0;
      rules_with_project_prefilters = 0;
      rules_with_file_prefilters = 0;
      rules_selected = 0;
      rules_matched = 0;
    }

  let combine (qprof1 : t) (qprof2 : t) : t =
    {
      project_level_time =
        qprof1.project_level_time +. qprof2.project_level_time;
      file_level_time = qprof1.file_level_time +. qprof2.file_level_time;
      rules = qprof1.rules + qprof2.rules;
      rules_with_project_prefilters =
        qprof1.rules_with_project_prefilters
        + qprof2.rules_with_project_prefilters;
      rules_with_file_prefilters =
        qprof1.rules_with_file_prefilters + qprof2.rules_with_file_prefilters;
      rules_selected = qprof1.rules_selected + qprof2.rules_selected;
      rules_matched = qprof1.rules_matched + qprof2.rules_matched;
    }

  (* Safe ratio calculation - returns 0.0 when denominator is 0 *)
  let safe_ratio numerator denominator =
    if Int.equal denominator 0 then 0.0
    else float_of_int numerator /. float_of_int denominator

  (* Convert internal counts to the external ratio format *)
  let to_ratio_stats (counts : t) : Semgrep_output_v1_t.prefiltering_stats =
    {
      project_level_time = counts.project_level_time;
      file_level_time = counts.file_level_time;
      rules_with_project_prefilters_ratio =
        safe_ratio counts.rules_with_project_prefilters counts.rules;
      rules_with_file_prefilters_ratio =
        safe_ratio counts.rules_with_file_prefilters counts.rules;
      rules_selected_ratio = safe_ratio counts.rules_selected counts.rules;
      rules_matched_ratio = safe_ratio counts.rules_matched counts.rules;
    }
end

type t = {
  parsing_stats : Parsing_stats.t;
  scanning_stats : Scanning_stats.t;
  matching_stats : Matching_stats.t;
  tainting_stats : Tainting_stats.t;
  prefiltering_stats : Prefiltering_stats.t;
}
[@@deriving show]

let zero =
  {
    parsing_stats = Parsing_stats.zero;
    scanning_stats = Scanning_stats.zero;
    matching_stats = Matching_stats.zero;
    tainting_stats = Tainting_stats.zero;
    prefiltering_stats = Prefiltering_stats.zero;
  }

let combine qprof1 qprof2 =
  {
    parsing_stats =
      Parsing_stats.combine qprof1.parsing_stats qprof2.parsing_stats;
    scanning_stats =
      Scanning_stats.combine qprof1.scanning_stats qprof2.scanning_stats;
    matching_stats =
      Matching_stats.combine qprof1.matching_stats qprof2.matching_stats;
    tainting_stats =
      Tainting_stats.combine qprof1.tainting_stats qprof2.tainting_stats;
    prefiltering_stats =
      Prefiltering_stats.combine qprof1.prefiltering_stats
        qprof2.prefiltering_stats;
  }

let combine_opt opt_qprof1 opt_qprof2 =
  match (opt_qprof1, opt_qprof2) with
  | None, None -> None
  | Some _, _
  | _, Some _ ->
      let qprof1 = opt_qprof1 ||| zero in
      let qprof2 = opt_qprof2 ||| zero in
      Some (combine qprof1 qprof2)

let map_opt f opt_qprof = Some (f (opt_qprof ||| zero))

let add_parse_time file parse_time qprof =
  {
    qprof with
    parsing_stats = Parsing_stats.update qprof.parsing_stats file parse_time;
  }

let add_run_time file run_time qprof =
  {
    qprof with
    scanning_stats = Scanning_stats.update qprof.scanning_stats file run_time;
  }

let add_match_time file rule_id match_time qprof =
  {
    qprof with
    matching_stats =
      Matching_stats.update qprof.matching_stats (file, rule_id) match_time;
  }

let add_taint_time file pos rule_id taint_time qprof =
  {
    qprof with
    tainting_stats =
      Tainting_stats.update qprof.tainting_stats (file, pos, rule_id) taint_time;
  }

let add_project_level_time time qprof =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        project_level_time = qprof.prefiltering_stats.project_level_time +. time;
      };
  }

let add_file_level_time time qprof =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        file_level_time = qprof.prefiltering_stats.file_level_time +. time;
      };
  }

let add_rules qprof n =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        rules = qprof.prefiltering_stats.rules + n;
      };
  }

let add_rules_with_project_prefilters qprof n =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        rules_with_project_prefilters =
          qprof.prefiltering_stats.rules_with_project_prefilters + n;
      };
  }

let add_rules_with_file_prefilters qprof n =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        rules_with_file_prefilters =
          qprof.prefiltering_stats.rules_with_file_prefilters + n;
      };
  }

let add_rules_selected qprof n =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        rules_selected = qprof.prefiltering_stats.rules_selected + n;
      };
  }

let add_rules_matched qprof n =
  {
    qprof with
    prefiltering_stats =
      {
        qprof.prefiltering_stats with
        rules_matched = qprof.prefiltering_stats.rules_matched + n;
      };
  }
