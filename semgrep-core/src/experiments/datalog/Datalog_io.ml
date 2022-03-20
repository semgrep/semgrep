(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module D = Datalog_fact
open Datalog_fact

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Writing/reading datalog facts on disk.
 *
 * This was started from pfff/h_program-lang/datalog_code.ml which
 * handled the BDDBDDB and lui-based datalog engines.
 * This file handles DOOP/Souffle.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_value = function
  | V x | F x | N x | I x -> spf "'%s'" x
  | Z i -> spf "%d" i

let csv_of_tuple xs = (xs |> List.map string_of_value |> Common.join ",") ^ "\n"

(*****************************************************************************)
(* Write *)
(*****************************************************************************)
let write_facts_for_doop facts dir =
  let facts = facts |> List.map D.meta_fact in
  let groups = facts |> Common.group_assoc_bykey_eff in
  groups
  |> List.iter (fun (table, tuples) ->
         let file = Filename.concat dir table ^ ".csv" in
         pr2 (spf "generating tuples for %s" file);
         Common.with_open_outfile file (fun (mypr, _chan) ->
             tuples |> List.iter (fun tuple -> mypr (csv_of_tuple tuple))))
