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
(*
   Parse and interpret '.semgrepignore' files in addition to '.gitignore'
   files.

   The patterns they contain specify file paths to exclude from Semgrep scans.

   See the ml file for compatibility issues.
*)

(*
   We have to support the legacy built-in semgrepignore patterns
   when scanning for source code but we want something different
   or empty when scanning for secrets.

   The 'Empty' case is useful for testing.
*)
type default_semgrepignore_patterns = Empty | Semgrep_scan_legacy
type exclusion_mechanism = { use_semgrepignore_files : bool }

(* ".semgrepignore" *)
val default_semgrepignore_filename : string

(*
   Initialize the data used to filter paths.
   The project_root path must exist. It is used to
   locate .gitignore and .semgrepignore files.

   This is an instanciation of Gitignore_filter.t specific to Semgrep.

   Use Git_project.find_project_root to determine the root of the
   git project.

   semgrepignore_filename: defaults to default_semgrepignore_filename
     = ".semgrepignore"
*)
val create :
  ?cli_patterns:string list ->
  ?semgrepignore_filename:string ->
  default_semgrepignore_patterns:default_semgrepignore_patterns ->
  exclusion_mechanism:exclusion_mechanism ->
  project_root:Fpath.t ->
  unit ->
  Gitignore_filter.t
