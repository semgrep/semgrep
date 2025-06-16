(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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
(** [SharedCounterTable] is a table of different counters. This is useful
    for if we want to keep track of global values by some key, such as some
    metrics *)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
type 'a t = { table : ('a, int) Hashtbl.t; mutex : Mutex.t }

let create size = { table = Hashtbl.create size; mutex = Mutex.create () }

let add_and_fetch counter_table key increment =
  let table = counter_table.table in
  let mutex = counter_table.mutex in
  Mutex.protect mutex (fun () ->
      let counter_opt = Hashtbl.find_opt table key in
      let counter = Option.value ~default:0 counter_opt in
      let new_value = counter + increment in
      Hashtbl.replace table key new_value;
      new_value)

let reset counter_table key =
  let table = counter_table.table in
  let mutex = counter_table.mutex in
  Mutex.protect mutex (fun () -> Hashtbl.replace table key 0)
