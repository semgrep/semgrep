open Lsp.Types
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/loginFinish"
let wait_before_retry_in_ms = 6 * 1000

(* let's give them a solid 3 minutes, since people can be slow, and somehow *)
(* this goes by way faster on Windows *)
let max_retries = 30

(*****************************************************************************)
(* Request parameters *)
(*****************************************************************************)

type t = { url : string; sessionId : string } [@@deriving yojson]

let of_jsonrpc_params params : (Uri.t * Uuidm.t) option =
  match params with
  | Some params -> (
      match of_yojson (Jsonrpc.Structured.yojson_of_t params) with
      | Error _ -> None
      | Ok { url; sessionId } ->
          Some (Uri.of_string url, Uuidm.of_string sessionId |> Option.get))
  | __else__ -> None

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let on_notification _server params : unit =
  (* Emulating a poor man's writer's monad, mixed with some LWT goodness. *)
  let ( let^ ) (x : (_, string) Result.t Lwt.t) f : unit Lwt.t =
    let%lwt result = x in
    match result with
    | Error s ->
        RPC_server.notify_show_message ~kind:MessageType.Error
          ("Failed to complete login process: " ^ s);
        Lwt.return ()
    | Ok y -> f y
  in
  match params with
  | None ->
      Logs.warn (fun m ->
          m "semgrep/loginFinish got no params but expected some")
  | Some _ ->
      (* All of this is side-effecting, so we can run it asynchronously, and
         return to the main event loop.
      *)
      Lwt.async (fun () ->
          let^ _url, sessionId =
            of_jsonrpc_params params
            |> Option.to_result ~none:"got invalid parameters"
            |> Lwt.return
          in
          let caps = Cap.network_caps_UNSAFE () in
          let^ token, _ =
            Semgrep_login.fetch_token_async ~min_wait_ms:wait_before_retry_in_ms
              ~max_retries
              ~wait_hook:(fun delay_ms ->
                Lwt_platform.sleep Float.(of_int delay_ms /. 1000.))
              caps sessionId
          in
          let caps = Auth.cap_token_and_network token caps in
          let^ _deployment =
            Semgrep_App.get_deployment_from_token_async caps
            |> Lwt.map (Option.to_result ~none:"failed to get deployment")
          in
          (* TODO: state.app_session.authenticate()
             basically, just add the token to the metrics once that exists
          *)
          RPC_server.notify_show_message ~kind:MessageType.Info
            "Successfully logged into Semgrep Code";
          let^ _deployment = Semgrep_login.save_token_async caps in
          Lwt.return ())
