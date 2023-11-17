open Cohttp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to perform http GET and POST requests.
 *
 * Below we separate the methods out by async (returns Lwt promise),
 * and sync (runs async method in lwt runtime)
 * This way we can use the async methods in the language server,
 * and other places too.
 *
 * Note that using [@@profiling] with xxx_async function is useless
 * as the actual computation is done in the caller doing the
 * Lwy_main.run
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type get_info = {
  response : Cohttp.Response.t;
  body : Cohttp_lwt.Body.t;
  code : int;
}

(*****************************************************************************)
(* Client *)
(*****************************************************************************)

(* Create a client reference so we can swap it out with a testing version *)
(*****************************************************************************)
(* Async *)
(*****************************************************************************)
let client_ref : (module Cohttp_lwt.S.Client) option ref = ref None

(* We use a functor here so that we can pass in what we use for the Lwt runtime. *)
(* This is platform dependent (JS vs Unix), so we can't just choose one *)
(* We could use a ref like above, but this doesn't need to be decided at runtime, only *)
(* at build, so I don't want to open the door to being able to change it at runtime *)
(* for no reason *)
(* We also can't just call Lwt_platform directly, as when compiling/linking a package *)
(* OCaml requires a choice of a virtual module implementation, and it can't defer *)
(* to the thing that's using the package. *)
module Make (Lwt_platform : sig
  val run : 'a Lwt.t -> 'a
end) =
struct
  let get_async ?(headers = []) url =
    Logs.debug (fun m -> m "GET on %s" (Uri.to_string url));
    (* This checks to make sure a client has been set *)
    (* Instead of defaulting to a client, as that can cause *)
    (* Hard to debug build and runtime issues *)
    let module Client =
      (val match !client_ref with
           | Some client -> client
           | None -> failwith "HTTP client not initialized")
    in
    let headers = Header.of_list headers in
    let%lwt response, orig_body = Client.get ~headers url in
    let%lwt body = Cohttp_lwt.Body.to_string orig_body in
    let code = response |> Response.status |> Code.code_of_status in
    match code with
    | _ when Code.is_success code ->
        Lwt.return (Ok (body, { code; response; body = orig_body }))
    | _ when Code.is_error code ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP GET failed: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (err, { code; response; body = orig_body }))
    | _ ->
        let code_str = Code.string_of_status response.status in
        let err = "HTTP GET unexpected response: " ^ code_str ^ ":\n" ^ body in
        Logs.debug (fun m -> m "%s" err);
        Lwt.return (Error (err, { code; response; body = orig_body }))

  let post_async ~body ?(headers = [ ("content-type", "application/json") ])
      ?(chunked = false) url =
    Logs.debug (fun m -> m "POST on %s" (Uri.to_string url));
    (* See comment above *)
    let module Client =
      (val match !client_ref with
           | Some client -> client
           | None -> failwith "HTTP client not initialized")
    in
    let headers = Header.of_list headers in
    let%lwt response, body =
      Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) ~chunked url
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

  (*****************************************************************************)
  (* Sync *)
  (*****************************************************************************)

  (* TODO: extend to allow to curl with JSON as answer *)
  let get ?headers url = Lwt_platform.run (get_async ?headers url) [@@profiling]

  let post ~body ?(headers = [ ("content-type", "application/json") ])
      ?(chunked = false) url =
    (* We add an exception handler to handle otherwise uncaught unix errors
       (e.g. ECONNREFUSED) and return a more helpful error message.

       Currently, we're observing high failure rates from our metrics endpoint
       with the corresponding error `Unix_error: Connection reset by peer read` when
       the server initiates a connection with SSL via TLS v1.2 instead of v1.3

       For investigative work, we're currently using an internal AWS Lambda URL instead
       of our human-friendly metrics endpoint. This is because the Lambda URL tends to
       get matched to a node with TLS v1.3 support, whereas our metrics endpoint for some
       reason almost exclusively is matched with a node that initiaties TLS v1.2 which
       is causing the connection reset error.

       This is a somewhat temporary workaround as even our Lambda URL will not always
       respond with TLS v1.3, as we are not guaranteed to hit the same node. Currently,
       AWS does not support specifying a minimum TLS version of v1.3, and we will need
       to figure out a better solution for ensuring reliable metrics delivery.
    *)
    Lwt_platform.run
      (Lwt.catch
         (fun () -> post_async ~body ~headers ~chunked url)
         (fun exn ->
           let err = Printexc.to_string exn in
           (* NOTE: the caller will have the responsibility to handle and log the error
              with the appropriate log level serverity (e.g. warn / error / app)
           *)
           Logs.debug (fun m ->
               m "send to '%s' failed: %s" (Uri.to_string url) err);
           Lwt.return (Error (0, err))))
  [@@profiling]
end
