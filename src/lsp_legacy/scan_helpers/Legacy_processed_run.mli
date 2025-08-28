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
val of_matches :
  ?skipped_fingerprints:string list ->
  ?only_git_dirty:bool ->
  ?git_ref:string ->
  Core_runner.result ->
  Semgrep_output_v1_t.cli_match list
(**  [of_matches ~only_git_dirty result] returns the list of cli matches from the
     result of a semgrep run. If [only_git_dirty] is [true], only the matches
     that are in files + lines that are git dirty are returned. If [git_ref] is set,
     then we will filter out matches that have been changed since that ref.
  *)
