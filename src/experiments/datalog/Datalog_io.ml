(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
open Fpath_.Operators
module D = Datalog_fact
open Datalog_fact

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Writing/reading datalog facts on disk.
 *
 * This was started from h_program-lang/datalog_code.ml which
 * handled the BDDBDDB and lui-based datalog engines.
 * This file handles DOOP/Souffle.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_value = function
  | V x
  | F x
  | N x
  | I x ->
      spf "'%s'" x
  | Z i -> spf "%d" i

let csv_of_tuple xs =
  (xs |> List_.map string_of_value |> String.concat ",") ^ "\n"

(*****************************************************************************)
(* Write *)
(*****************************************************************************)
let write_facts_for_doop facts dir =
  let facts = facts |> List_.map D.meta_fact in
  let groups = facts |> Assoc.group_assoc_bykey_eff in
  groups
  |> List.iter (fun (table, tuples) ->
         let file = dir / (table ^ ".csv") in
         (* nosemgrep: no-logs-in-library *)
         Logs.info (fun m -> m "generating tuples for %s" !!file);
         UFile.with_open_out file (fun (mypr, _chan) ->
             tuples |> List.iter (fun tuple -> mypr (csv_of_tuple tuple))))
