open Cohttp

(* Below we separate the methods out by async (returns Lwt promise),
   and sync (runs async method in lwt runtime)
*)
(* This way we can use the async methods in the language server,
   and other places too
*)

(*****************************************************************************)
(* Client *)
(*****************************************************************************)

(* Create a client reference so we can swap it out with a testing version *)
let client_ref = ref (module Cohttp_lwt_unix.Client : Cohttp_lwt.S.Client)

(*****************************************************************************)
(* Async *)
(*****************************************************************************)

let get_async ?(headers = []) url =
  let module Client = (val !client_ref) in
  let headers = Header.of_list headers in
  let%lwt response, body = Client.get ~headers url in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let code = response |> Response.status |> Code.code_of_status in
  match code with
  | _ when Code.is_success code -> Lwt.return (Ok body)
  | _ when Code.is_error code ->
      let code_str = Code.string_of_status response.status in
      let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
      Logs.debug (fun m -> m "%s" err);
      Lwt.return (Error err)
  | _ ->
      let code_str = Code.string_of_status response.status in
      let err = "HTTP GET unexpected response: " ^ code_str ^ ":\n" ^ body in
      Logs.debug (fun m -> m "%s" err);
      Lwt.return (Error err)
  [@@profiling]

let post_async ~body ?(headers = [ ("content-type", "application/json") ]) url =
  let module Client = (val !client_ref) in
  let headers = Header.of_list headers in
  let%lwt response, body =
    Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) url
  in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let code = response |> Response.status |> Code.code_of_status in
  match code with
  | _ when Code.is_success code -> Lwt.return (Ok body)
  | _ when Code.is_error code ->
      let code_str = Code.string_of_status response.status in
      let err = "HTTP POST failed: " ^ code_str ^ ":\n" ^ body in
      Logs.debug (fun m -> m "%s" err);
      Lwt.return (Error (code, err))
  | _ ->
      let code_str = Code.string_of_status response.status in
      let err = "HTTP POST unexpected response: " ^ code_str ^ ":\n" ^ body in
      Logs.debug (fun m -> m "%s" err);
      Lwt.return (Error (code, err))
  [@@profiling]

(*****************************************************************************)
(* Sync *)
(*****************************************************************************)

(* TODO: extend to allow to curl with JSON as answer *)
let get ?headers url = Lwt_main.run (get_async ?headers url) [@@profiling]

let post ~body ?(headers = [ ("content-type", "application/json") ]) url =
  Lwt_main.run (post_async ~body ~headers url)
  [@@profiling]
