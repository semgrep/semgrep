open Lwt.Syntax
open Cohttp_lwt_unix

let get url =
  let promise =
    let* _resp, body = Client.get url in
    Cohttp_lwt.Body.to_string body
  in
  let content = Lwt_main.run promise in
  content
