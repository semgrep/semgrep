(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* TODO: rename this module to indicate that it only works on a git repo *)
(* TODO: provide a function to obtain the list of target files. *)

type diff_scan_func = Fppath.t list -> Rule.rules -> Core_result.result_or_exn

(* Execute the engine again on the baseline checkout, utilizing only
 * the files and rules linked with matches from the head checkout
 *  scan. Subsequently, eliminate any previously identified matches
 * from the results of the head checkout scan.
 * Note: uses Git_wrapper.run_with_worktree (which does chdir and
 * creates tmp dirs) and recomputes some targets for interfile.
 *)

val scan_baseline :
  Profiler.t ->
  string (* baseline commit *) ->
  Rule.rules ->
  diff_scan_func ->
  Core_result.result_or_exn
