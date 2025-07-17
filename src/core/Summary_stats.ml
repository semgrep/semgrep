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

module Out_v1 = Semgrep_output_v1_j

(*****************************************************************************)
(* Time *)
(*****************************************************************************)

type 'key time = { key : 'key; time : float } [@@deriving show]

let to_file_time (t : Fpath.t time) : Semgrep_output_v1_t.file_time =
  Semgrep_output_v1_t.{ fpath = t.key; ftime = t.time }

let to_file_rule_time (t : (Fpath.t * Rule_ID.t) time) :
    Semgrep_output_v1_t.file_rule_time =
  let fpath, rule_id = t.key in
  Semgrep_output_v1_t.{ fpath; rule_id; time = t.time }

let to_def_rule_time (t : (Fpath.t * Pos.t * Rule_ID.t) time) :
    Semgrep_output_v1_t.def_rule_time =
  let fpath, pos, rule_id = t.key in
  Semgrep_output_v1_t.{ fpath; fline = pos.line; rule_id; time = t.time }

(*****************************************************************************)
(* Stats *)
(*****************************************************************************)

type 'key stats = {
  count : int;
  sum : float;
  mean : float;
  m2 : float;
  very_slow_count : int;
  very_slow_sum : float;
  very_slow : 'key time list;
}
[@@deriving show]

module type Key = sig
  type t [@@deriving show]

  val very_slow_threshold : float
  val very_slow_top_size : int
end

module type S = sig
  type key
  type t = key stats [@@deriving show]

  val zero : t
  val update : t -> key -> float -> t
  val combine : t -> t -> t

  val to_output_v1 :
    to_out_time:(key time -> 'out_time) ->
    key stats ->
    Out_v1.summary_stats * Out_v1.very_slow_stats * 'out_time list
end

module Make (Key : Key) : S with type key = Key.t = struct
  type key = Key.t [@@deriving show]
  type t = key stats [@@deriving show]

  (***********************************************)
  (* Very-slow helpers *)
  (***********************************************)

  (* Not tail-rec but we assume 'ys' is rather short. *)
  let rec insert_into_sorted_asc (x : key time) keys_and_times =
    match keys_and_times with
    | [] -> [ x ]
    | (y : key time) :: _ when y.time > x.time -> x :: keys_and_times
    | y :: ys -> y :: insert_into_sorted_asc x ys

  let top_very_slow__update very_slow (x : key time) =
    let n = List.length very_slow in
    if x.time > Key.very_slow_threshold then
      match very_slow with
      | __any__ when n < Key.very_slow_top_size ->
          insert_into_sorted_asc x very_slow
      | (y : key time) :: ys when n >= Key.very_slow_top_size && x.time > y.time
        ->
          insert_into_sorted_asc x ys
      | _ :: _
      | [] ->
          very_slow
    else very_slow

  let top_very_slow__combine vslow1 vslow2 =
    vslow1
    |> List.rev_append (List.rev vslow2)
    |> List.sort_uniq (fun (x : key time) (y : key time) ->
           Float.compare x.time y.time)
    |> List.rev
    (* Reverse to descending order to take the slowest ones. *)
    |> List_.take_safe Key.very_slow_top_size
    (* Back to ascending order *)
    |> List.rev

  (***********************************************)
  (* Public *)
  (***********************************************)

  let zero =
    {
      count = 0;
      sum = 0.0;
      mean = 0.0;
      m2 = 0.0;
      very_slow_count = 0;
      very_slow_sum = 0.0;
      very_slow = [];
    }

  (* Welford's online algorithm *)
  let update stats key x =
    let count = stats.count + 1 in
    let sum = stats.sum +. x in
    let delta = x -. stats.mean in
    let mean = stats.mean +. (delta /. float_of_int count) in
    let delta2 = x -. mean in
    let m2 = stats.m2 +. (delta *. delta2) in
    let very_slow_count, very_slow_sum =
      if x > Key.very_slow_threshold then
        (stats.very_slow_count + 1, stats.very_slow_sum +. x)
      else (stats.very_slow_count, stats.very_slow_sum)
    in
    let very_slow = top_very_slow__update stats.very_slow { key; time = x } in
    { count; sum; mean; m2; very_slow_count; very_slow_sum; very_slow }
  [@@profile]

  let combine stats1 stats2 =
    let count = stats1.count + stats2.count in
    if count = 0 then zero
    else
      let sum = stats1.sum +. stats2.sum in
      let delta = stats2.mean -. stats1.mean in
      let mean =
        stats1.mean +. (delta *. float_of_int stats2.count /. float_of_int count)
      in
      let m2 =
        stats1.m2 +. stats2.m2
        +. delta *. delta *. float_of_int stats1.count
           *. float_of_int stats2.count /. float_of_int count
      in
      let very_slow_count = stats1.very_slow_count + stats2.very_slow_count in
      let very_slow_sum = stats1.very_slow_sum +. stats2.very_slow_sum in
      let very_slow =
        top_very_slow__combine stats1.very_slow stats2.very_slow
      in
      { count; sum; mean; m2; very_slow_count; very_slow_sum; very_slow }

  let to_output_v1 ~(to_out_time : Key.t time -> _) stats :
      Out_v1.summary_stats * Out_v1.very_slow_stats * _ list =
    let summary_stats =
      let std_dev =
        if stats.count < 2 then 0.0 else stats.m2 /. float_of_int stats.count
      in
      Out_v1.{ mean = stats.mean; std_dev }
    in
    let very_slow_stats =
      Out_v1.
        {
          count_ratio =
            (if stats.count > 0 then
               float_of_int stats.very_slow_count /. float_of_int stats.count
             else 0.0);
          time_ratio =
            (if stats.sum > Float.epsilon then stats.very_slow_sum /. stats.sum
             else 0.0);
        }
    in
    let top_very_slow = stats.very_slow |> List_.map to_out_time in
    (summary_stats, very_slow_stats, top_very_slow)
end
