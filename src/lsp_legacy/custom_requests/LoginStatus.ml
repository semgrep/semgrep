let meth = "semgrep/loginStatus"

let on_request (session : Session.t) id _params : Session.t * Lsp_.Reply.t =
  ( session,
    Lsp_.Reply.now
      (Lsp_.respond_json id
         (`Assoc [ ("loggedIn", `Bool (Semgrep_login.is_logged_in_weak ())) ]))
  )
