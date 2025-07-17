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

type t = {
  parsing_stats : Parsing_stats.t;
  scanning_stats : Scanning_stats.t;
  matching_stats : Matching_stats.t;
  tainting_stats : Tainting_stats.t;
}
[@@deriving show]

let zero =
  {
    parsing_stats = Parsing_stats.zero;
    scanning_stats = Scanning_stats.zero;
    matching_stats = Matching_stats.zero;
    tainting_stats = Tainting_stats.zero;
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
