open Lsp.Types
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

let logger = Logging.get_logger [ __MODULE__ ]
let meth = "semgrep/loginFinish"

(*

let wait_before_retry_in_sec = 6
let max_retries = 30

def m_semgrep__login_finish(self, url: str, sessionId: str) -> None:
  """Called by client to finish login to Semgrep App and save token"""
  self.notify_show_message(3, f"Waiting for login to Semgrep Code at {url}...")

  state = get_state()
  for _ in range(MAX_RETRIES):
      r = state.app_session.post(
          f"{state.env.semgrep_url}/api/agent/tokens/requests",
          json={"token_request_key": sessionId},
      )
      if r.status_code == 200:
          as_json = r.json()
          login_token = as_json.get("token")
          state = get_state()
          if login_token is not None and auth.get_deployment_from_token(
              login_token
          ):
              auth.set_token(login_token)
              state = get_state()
              state.app_session.authenticate()
              self.notify_show_message(
                  3, f"Successfully logged in to Semgrep Code"
              )
          else:
              self.notify_show_message(1, f"Failed to log in to Semgrep Code")
          return
      elif r.status_code != 404:
          self.notify_show_message(
              1,
              f"Unexpected failure from {state.env.semgrep_url}: status code {r.status_code}; please contact support@semgrep.com if this persists",
          )

      time.sleep(WAIT_BETWEEN_RETRY_IN_SEC)
*)

(*****************************************************************************)
(* Request parameters *)
(*****************************************************************************)

type t = { url : string; sessionId : string } [@@deriving yojson]

let of_jsonrpc_params params : t option =
  match params with
  | Some params ->
      of_yojson (Jsonrpc.Structured.yojson_of_t params) |> Result.to_option
  | __else__ -> None

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let on_notification server params : RPC_server.t option =
  let ( let^ ) x f =
    match x with
    | Error s ->
        RPC_server.notify_show_message server ~kind:MessageType.Error
          ("semgrep/loginFinish failed: " ^ s);
        None
    | Ok y -> f y
  in
  match params with
  | None ->
      logger#error "semgrep/loginFinish got no params but expected some";
      None
  | Some _ ->
      let^ { url; sessionId } =
        of_jsonrpc_params params |> Option.to_result ~none:"got invalid params"
      in
      let url = Uuidm.of_string url |> Option.get in
      let sessionId = sessionId |> Uri.of_string in
      let^ token, _ = Semgrep_login.fetch_token (url, sessionId) in
      let^ _deployment =
        Semgrep_App.get_deployment_from_token token
        |> Option.to_result ~none:"failed to get deployment"
      in
      let notifs =
        Lsp.Server_notification.ShowMessage
          {
            ShowMessageParams.message = "Successfully logged in to Semgrep Code";
            type_ = MessageType.Info;
          }
      in
      RPC_server.batch_notify server [ notifs ];
      let^ () = Semgrep_login.save_token token in
      (* TODO: state.app_session.authenticate() *)
      Some server
