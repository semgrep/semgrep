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

(* TODO: Make a functor ? *)

module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let very_slow_threshold = 0.5 (* seconds *)
let very_slow_count = 10

type file_time = Out.file_time [@@deriving show]

type t = {
  count : int;
  sum : float;
  mean : float;
  m2 : float;
  (* Top 'very_slow_count' slowest files, that take at least 'very_slow_threshold'
    seconds to parse. *)
  very_slow : file_time list;
}
[@@deriving show]

(*****************************************************************************)
(* Very-slow helpers *)
(*****************************************************************************)

(* Not tail-rec but we assume 'ys' is rather short. *)
let rec insert_into_sorted_asc (x : Out.file_time) files_and_times =
  match files_and_times with
  | [] -> [ x ]
  | (y : Out.file_time) :: _ when y.ftime > x.ftime -> x :: files_and_times
  | y :: ys -> y :: insert_into_sorted_asc x ys

let update_very_slow very_slow (x : file_time) =
  let n = List.length very_slow in
  if x.ftime > very_slow_threshold then
    match very_slow with
    | __any__ when n < very_slow_count -> insert_into_sorted_asc x very_slow
    | (y : Out.file_time) :: ys when n >= very_slow_count && x.ftime > y.ftime
      ->
        insert_into_sorted_asc x ys
    | _ :: _
    | [] ->
        very_slow
  else very_slow

let combine_very_slow vslow1 vslow2 =
  vslow1
  |> List.rev_append (List.rev vslow2)
  |> List.sort_uniq (fun (x : Out.file_time) (y : Out.file_time) ->
         Float.compare x.ftime y.ftime)
  |> List.rev
  (* Reverse to descending order to take the slowest ones. *)
  |> List_.take_safe very_slow_count
  (* Back to ascending order *)
  |> List.rev

(*****************************************************************************)
(* Public *)
(*****************************************************************************)

let zero = { count = 0; sum = 0.0; mean = 0.0; m2 = 0.0; very_slow = [] }

(* Welford's online algorithm *)
let update stats fpath x =
  let count = stats.count + 1 in
  let sum = stats.sum +. x in
  let delta = x -. stats.mean in
  let mean = stats.mean +. (delta /. float_of_int count) in
  let delta2 = x -. mean in
  let m2 = stats.m2 +. (delta *. delta2) in
  let very_slow = update_very_slow stats.very_slow { fpath; ftime = x } in
  { count; sum; mean; m2; very_slow }
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
    let very_slow = combine_very_slow stats1.very_slow stats2.very_slow in
    { count; sum; mean; m2; very_slow }
[@@profile]
