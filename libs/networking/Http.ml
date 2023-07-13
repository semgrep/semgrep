open Piaf
 (* TODO: extend to allow to curl with JSON as answer *)
(* This deadcode is the old impl get/post via happy eyeballs;
 why is it still here? idk
 let get ?headers url =
  let bodyf _ acc data = Lwt.return (acc ^ data) in
let promise = request ~follow_redirect:true ~happy_eyeballs ?headers (Uri.to_string url) bodyf "" in
  let r = Lwt_main.run promise in
  match r with 
  | Ok (response, content) when Status.is_successful response.status ->
      Ok content
  | Ok (response, _) ->
      Error
        ("HTTP request failed, server response "
        ^ Status.to_string response.status)
  | Error (`Msg msg) ->
      let err = "HTTP request failed: " ^ msg in
      Logs.debug (fun m -> m "%s" err);
      Error err
  [@@profiling]

let post ~body ?(headers = [ ("content-type", "application/json") ]) url =
  let bodyf _ acc data = Lwt.return (acc ^ data) in
  let promise =
    request ~happy_eyeballs ~meth:`POST ~headers ~body (Uri.to_string url) bodyf
      ""
  in
  let r = Lwt_main.run promise in
  match r with
  | Ok (response, content) when Status.is_successful response.status ->
      Ok content
  | Ok (response, _) ->
      Error
        ( Status.to_code response.status,
          "HTTP request failed, server response "
          ^ Status.to_string response.status )
  | Error (`Msg msg) ->
      let err = "HTTP request failed: " ^ msg in
      Logs.debug (fun m -> m "%s" err);
      Error (-1, err)
  [@@profiling
*)
let get ?headers url =
  Lwt_result.(
  Client.Oneshot.get ?headers url
    >>=
    fun x ->
       Body.to_string x.body
)
|> Lwt_main.run
|> (function Error _ -> Error "get failed" | Ok a -> Ok a)

let post ~body ?(headers = [ ("content-type", "application/json") ]) url =
  Lwt_result.(
  Client.Oneshot.post ~headers ~body:(Body.of_string body) url
    >>=
    fun x ->
       Body.to_string x.body
)
|> Lwt_main.run
|> (function Error _ -> Error (-1,"post failed") | Ok a -> Ok a)
