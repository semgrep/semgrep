module SN = Lsp.Server_notification
module Conv = Legacy_convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/loginStart"

let on_request (session : Legacy_session.t) (req_id : Jsonrpc.Id.t) _params :
    Legacy_session.t * Legacy_lsp_.Reply.t =
  let session_id, uri = Semgrep_login.make_login_url () in
  ( session,
    Legacy_lsp_.Reply.now
      (Legacy_lsp_.respond_json req_id
         (`Assoc
            [
              ("url", `String (Uri.to_string uri));
              ("sessionId", `String (Uuidm.to_string session_id));
            ])) )
