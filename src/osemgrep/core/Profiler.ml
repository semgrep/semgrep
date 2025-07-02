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

type t = { ht : (string, value) Hashtbl.t; mtx : Mutex.t }
and value = Start of float | Recorded of float

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let make () = { ht = Hashtbl.create 0x100; mtx = Mutex.create () }

let start { ht; mtx } ~name =
  Mutex.protect mtx @@ fun () ->
  match Hashtbl.find_opt ht name with
  | Some (Start start_time) ->
      let now = Unix.gettimeofday () in
      Hashtbl.replace ht name (Recorded (now -. start_time))
  | Some (Recorded _) -> invalid_arg "%s was already profiled"
  | None ->
      let now = Unix.gettimeofday () in
      Hashtbl.add ht name (Start now)

let stop profiler ~name =
  let { ht; mtx } = profiler in
  Mutex.protect mtx @@ fun () ->
  match Hashtbl.find_opt ht name with
  | Some (Start _) ->
      let now = Unix.gettimeofday () in
      Hashtbl.add ht name (Start now)
  | Some (Recorded _) ->
      invalid_arg (spf "Profiler.stop: %s already recorded" name)
  | None -> invalid_arg (spf "Profiler.stop: %s does not exist" name)

let stop_ign profiler ~name =
  try stop profiler ~name with
  | _ -> ()

let record profiler ~name fn =
  let { ht; mtx } = profiler in
  let t0 = Unix.gettimeofday () in
  let finally () =
    let t1 = Unix.gettimeofday () in
    Mutex.protect mtx @@ fun () -> Hashtbl.add ht name (Recorded (t1 -. t0))
  in
  Common.protect ~finally fn

let dump { ht; mtx } =
  Mutex.protect mtx @@ fun () ->
  Hashtbl.fold
    (fun name value acc ->
      match value with
      | Recorded time -> (name, time) :: acc
      | _ -> acc)
    ht []
