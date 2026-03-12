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
open Common

(* GitHub REST API *)

let find_branchoff_point_async ~gh_token ~api_url ~repo_name ~base_branch_hash
    head_branch_hash =
  let str = Auth.string_of_token gh_token in
  let headers = [ ("Authorization", spf "Bearer %s" str) ] in
  let%lwt response =
    Http_helpers.get ~headers
      (Uri.of_string
         (spf "%s/repos/%s/compare/%s...%s" (Uri_.show api_url) repo_name
            (Fmt_.to_show Digestif.SHA1.pp base_branch_hash)
            (Fmt_.to_show Digestif.SHA1.pp head_branch_hash)))
  in
  match response with
  | Ok { body = Ok body; _ } ->
      let body = body |> Yojson.Basic.from_string in
      let commit =
        Option.bind
          Glom.(
            get_and_coerce_opt string body [ k "merge_base_commit"; k "sha" ])
          Digestif.SHA1.of_hex_opt
      in
      Lwt.return commit
  | Ok { body = Error e; code; _ } ->
      Logs.err (fun m -> m "Github API returned an error code %d: %s" code e);
      Lwt.return_none
  | Error e ->
      Logs.err (fun m -> m "Failed to fetch branch off point: %s" e);
      Lwt.return_none
