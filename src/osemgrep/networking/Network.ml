open Http_lwt_client

(* TODO: extend to allow to curl with JSON as answer *)
let get url =
  let bodyf _ acc data = Lwt.return (acc ^ data) in
  let promise = request (Uri.to_string url) bodyf "" in
  let r = Lwt_main.run promise in
  match r with
  | Ok (response, content)
    when Status.is_successful response.status ->
    Ok content
  | Ok (response, _) ->
    Error ("HTTP request failed, server response " ^
           Status.to_string response.status)
  | Error `Msg msg -> Error ("HTTP request failed: " ^ msg)
  [@@profiling]
