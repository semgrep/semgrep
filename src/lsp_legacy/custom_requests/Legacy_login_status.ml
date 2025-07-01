let meth = "semgrep/loginStatus"

let on_request (session : Legacy_session.t) id _params :
    Legacy_session.t * Legacy_lsp_.Reply.t =
  ( session,
    Legacy_lsp_.Reply.now
      (Legacy_lsp_.respond_json id
         (`Assoc [ ("loggedIn", `Bool (Semgrep_login.is_logged_in_weak ())) ]))
  )
