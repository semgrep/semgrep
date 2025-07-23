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
type ('a, 'b) t = {
  table : ('a, 'b) Hashtbl.t;
  mutex : Mutex.t;
  add : 'b -> 'b -> 'b;
  default : 'b;
}

let create ~default ~add size =
  { table = Hashtbl.create size; mutex = Mutex.create (); add; default }

(* Useful for metric up/down counters *)
let create_int_table size = create ~default:0 ~add:( + ) size
let create_float_table size = create ~default:0.0 ~add:( +. ) size

(* Useful for metric histograms*)
let create_float_list_table size = create ~default:[] ~add:( @ ) size

let add_and_fetch counter_table key increment =
  let table = counter_table.table in
  let mutex = counter_table.mutex in
  let add = counter_table.add in
  let default = counter_table.default in
  Mutex.protect mutex (fun () ->
      let counter_opt = Hashtbl.find_opt table key in
      let counter = Option.value ~default counter_opt in
      let new_value = add counter increment in
      Hashtbl.replace table key new_value;
      new_value)

let reset counter_table key =
  let table = counter_table.table in
  let mutex = counter_table.mutex in
  Mutex.protect mutex (fun () ->
      Hashtbl.replace table key counter_table.default)
