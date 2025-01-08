(* Romain Calascibetta
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A really basic profiler.
 *
 * TODO: diff with libs/profiling/? Worth yet another profiling lib?
 * or was it written to match what was done in pysemgrep?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = (string, value) Hashtbl.t
and value = Start of float | Recorded of float

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let make () = Hashtbl.create 0x100

let start profiler ~name =
  match Hashtbl.find_opt profiler name with
  | Some (Start start_time) ->
      let now = Unix.gettimeofday () in
      Hashtbl.replace profiler name (Recorded (now -. start_time))
  | Some (Recorded _) -> invalid_arg "%s was already profiled"
  | None ->
      let now = Unix.gettimeofday () in
      Hashtbl.add profiler name (Start now)

let stop profiler ~name =
  match Hashtbl.find_opt profiler name with
  | Some (Start _) -> start profiler ~name
  | Some (Recorded _) ->
      invalid_arg (spf "Profiler.stop: %s already recorded" name)
  | None -> invalid_arg (spf "Profiler.stop: %s does not exist" name)

let stop_ign profiler ~name =
  try stop profiler ~name with
  | _ -> ()

let record profiler ~name fn =
  let t0 = Unix.gettimeofday () in
  let finally () =
    let t1 = Unix.gettimeofday () in
    Hashtbl.add profiler name (Recorded (t1 -. t0))
  in
  Common.protect ~finally fn

let dump profiler =
  Hashtbl.fold
    (fun name value acc ->
      match value with
      | Recorded time -> (name, time) :: acc
      | _ -> acc)
    profiler []
