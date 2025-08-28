(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Collect information about the project from the environment *)

(* What is sent to the Semgrep backend in the scan_request. This is now
 * defined in semgrep_output_v1.atd so it can be reused in the backend.
 *)
type t = Semgrep_output_v1_t.project_metadata

(* a few helpers *)
val get_url_from_sstp_url : string option -> Uri.t option
val get_repo_name_from_repo_url : string option -> string option
