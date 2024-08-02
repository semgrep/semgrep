module SN = Lsp.Server_notification
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/login"

let on_request (_session : Session.t) __params : Yojson.Safe.t option =
  let id, uri = Semgrep_login.make_login_url () in
  Some
    (`Assoc
      [
        ("url", `String (Uri.to_string uri));
        ("sessionId", `String (Uuidm.to_string id));
      ])
