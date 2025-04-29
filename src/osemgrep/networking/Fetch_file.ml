(** Fetch file content from a URL. Returns None if the request fails for any
 * reason. *)
let fetch_file_async caps (source_url : Uri.t) : string option Lwt.t =
  let content =
    match%lwt Http_helpers.get ~headers:[] caps#network source_url with
    | Ok { body = Ok body; _ } -> Lwt.return (Some body)
    | Ok { body = Error _; _ } -> Lwt.return_none
    | Error _ -> Lwt.return_none
  in
  content
