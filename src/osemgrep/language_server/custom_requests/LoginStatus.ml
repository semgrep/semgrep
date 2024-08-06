module SN = Lsp.Server_notification
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/loginStatus"

let on_request (_session : Session.t) _params : Yojson.Safe.t option =
  Some (`Assoc [ ("loggedIn", `Bool (Semgrep_settings.is_authenticated ())) ])
