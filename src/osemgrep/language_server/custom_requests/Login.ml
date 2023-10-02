open Lsp.Types
module SN = Lsp.Server_notification
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

let logger = Logging.get_logger [ __MODULE__ ]
let meth = "semgrep/login"

let on_request (server : RPC_server.t) params : Yojson.Safe.t option =
  match params with
  | None ->
      if Semgrep_login.is_logged_in () then (
        let notifs =
          SN.ShowMessage
            {
              ShowMessageParams.message = "Already logged in to Semgrep Code";
              type_ = MessageType.Info;
            }
        in
        RPC_server.batch_notify server [ notifs ];
        None)
      else
        let id, uri = Semgrep_login.make_login_url () in
        Some
          (`Assoc
            [
              ("url", `String (Uri.to_string uri));
              ("sessionId", `String (Uuidm.to_string id));
            ])
  | Some _ ->
      logger#error "semgrep/login got params but expected none";
      None
