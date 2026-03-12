(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Run jobs in parallel, using number of cores specified with -j.
 *
 * Note that the jobs are currently run in forked process, so while
 * the job will inherit the memory from the parent process, any modification
 * in the child process of the memory (e.g., modifying a global hash table)
 * will not be seen by the parent! So the job function should be a pure
 * function!
 * See: Parmap_.mli for why this returns a result list!
 *)
val map_targets__run_in_forked_process_do_not_modify_globals :
  num_jobs:int ->
  (Target.t -> 'a) ->
  (* job function *) Target.t list ->
  ('a, Target.t * Core_error.t) result list
