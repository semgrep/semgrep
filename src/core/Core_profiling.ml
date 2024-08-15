(* Emma Jin and Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Profiling information on rule matching and target parsing.
 *
 * Note that the extra information can take a lot of memory, which scales
 * with both the number of rules and files. On large repos, this used to
 * be (and may still be) the most significant factor driving semgrep's
 * memory consumption. Therefore, if the parsing times (for example) are
 * not being used, we don't want to save them.
 *
 * For example, on the semgrep-pro repo itself, running semgrep with
 * 4000 rules on 22000 files can lead to a JSON core_output of
 * 650 MB! compared to 5 MB without the profiling info.
 * This puts lots of stress in semgrep-core when generating this
 * huge JSON and also in pysemgrep when reading it (and later
 * when sending some of it to our metrics backend).
 *
 * Like for Core_error.ml and Core_result.ml, this "core" profiling
 * is eventually converted in semgrep_output_v1.core_timing,
 * which is then transformed in ProfilingData in profiling.py and
 * finally transformed in a semgrep_output_v1.profile type.
 * There's also Profiler.ml in osemgrep, and poor man profiler
 * stuff in libs/profiling/profiling.ml accessed via --profile
 * LATER: remove some intermediate types.
 *)

(*****************************************************************************)
(* Global *)
(*****************************************************************************)

(* Set by -json_time in semgrep-core or --time in pysemgrep/osemgrep to save
 * profiling info. Those profiling info found their way then in our metrics.
 *)
let profiling = ref false
let profiling_opt prof = if !profiling then Some prof else None

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Many of the types below are created to collect profiling information in as
 * well-typed a manner as possible. Creating a type for practically every
 * stage that reports matches is annoying, but prevents us from relying on
 * dummy values or unlabeled tuples.
 *)

(* general type *)
type times = { parse_time : float; match_time : float }

(* Save time information as we run each rule *)
type rule_profiling = {
  rule_id : Rule_ID.t;
  rule_parse_time : float;
  rule_match_time : float;
}
[@@deriving show]

(* Save time information as we run each file *)
type file_profiling = {
  file : Fpath.t;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type partial_profiling = {
  p_file : Fpath.t;
  p_rule_times : rule_profiling list;
}
[@@deriving show]

(* Profiling information for all the rules and targets.
 * old: was called Report.final_profiling
 *)
type t = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  (* This is meant to represent the maximum amount of memory used by
     Semgrep during the course of its execution.

     This is useful to emit with the other profiling data for telemetry
     purposes, particuarly as it relates to measuring memory management
     with DeepSemgrep.

     It's not important that this number be incredibly precise, but
     measuring general trends is useful for ascertaining our memory
     usage.
  *)
  max_memory_bytes : int;
}
[@@deriving show]

(*****************************************************************************)
(* Merge helpers *)
(*****************************************************************************)

(* used in pro engine e.g. when merging secret mode results *)
let merge a b : t =
  {
    rules = a.rules @ b.rules;
    rules_parse_time = a.rules_parse_time +. b.rules_parse_time;
    file_times = a.file_times @ b.file_times;
    max_memory_bytes = Int.max a.max_memory_bytes b.max_memory_bytes;
  }

let add_times (a : times) (b : times) : times =
  {
    match_time = a.match_time +. b.match_time;
    parse_time = a.parse_time +. b.parse_time;
  }

(*****************************************************************************)
(* Create empty versions of profiling objects *)
(*****************************************************************************)

let empty_partial_profiling file : partial_profiling =
  { p_file = file; p_rule_times = [] }

let empty_rule_profiling (rule : Rule.t) : rule_profiling =
  { rule_id = fst rule.Rule.id; rule_parse_time = 0.0; rule_match_time = 0.0 }
