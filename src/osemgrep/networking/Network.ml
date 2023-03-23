open Lwt.Syntax
open Cohttp_lwt_unix

(* TODO: extend to allow to curl with JSON as answer *)
let get url =
  let promise =
    let* _resp, body = Client.get url in
    Cohttp_lwt.Body.to_string body
  in
  let content = Lwt_main.run promise in
  content
  [@@profiling]
