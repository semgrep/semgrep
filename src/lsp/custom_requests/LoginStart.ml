module SN = Lsp.Server_notification
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/loginStart"

let on_request (session : Session.t) (req_id : Jsonrpc.Id.t) _params :
    Session.t * Lsp_.Reply.t =
  let session_id, uri = Semgrep_login.make_login_url () in
  ( session,
    Lsp_.Reply.now
      (Lsp_.respond_json req_id
         (`Assoc
           [
             ("url", `String (Uri.to_string uri));
             ("sessionId", `String (Uuidm.to_string session_id));
           ])) )
