module SN = Lsp.Server_notification
module Conv = Convert_utils
module Out = Semgrep_output_v1_t

let meth = "semgrep/loginStatus"

let on_request (_server : RPC_server.t) _params : Yojson.Safe.t option =
  Some (`Bool (Semgrep_login.is_logged_in ()))
