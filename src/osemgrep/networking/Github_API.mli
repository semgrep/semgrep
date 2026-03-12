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
(* GitHub REST API *)

val find_branchoff_point_async :
  gh_token:Auth.token ->
  api_url:Uri.t ->
  repo_name:string ->
  base_branch_hash:Digestif.SHA1.t ->
  Digestif.SHA1.t ->
  Digestif.SHA1.t option Lwt.t
