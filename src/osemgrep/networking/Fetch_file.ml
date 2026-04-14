(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** Fetch file content from a URL. Returns None if the request fails for any
   reason. *)
let fetch_file_async (source_url : Uri.t) : string option Lwt.t =
  let content =
    match%lwt Http_helpers.get ~headers:[] source_url with
    | Ok { body = Ok body; _ } -> Lwt.return (Some body)
    | Ok { body = Error _; _ } -> Lwt.return_none
    | Error _ -> Lwt.return_none
  in
  content
