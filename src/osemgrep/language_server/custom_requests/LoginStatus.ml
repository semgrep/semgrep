let meth = "semgrep/loginStatus"

let on_request (_session : Session.t) _params : Yojson.Safe.t option =
  Some (`Assoc [ ("loggedIn", `Bool (Semgrep_login.is_logged_in_weak ())) ])
