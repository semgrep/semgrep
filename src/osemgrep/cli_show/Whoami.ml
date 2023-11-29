module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

type identity_kind = Identity | Deployment

let print (kind : identity_kind) : Exit_code.t =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | Some token ->
      (match kind with
      | Identity ->
          let id = Lwt_platform.run (Semgrep_App.get_identity_async ~token) in
          Logs.app (fun m ->
              m "%s You are logged in as %s" (Logs_.success_tag ()) id)
      | Deployment -> (
          let (x : OutJ.deployment_config option) =
            Lwt_platform.run
              (Semgrep_App.get_deployment_from_token_async ~token)
          in
          match x with
          | None -> failwith "no deployment_config"
          | Some x ->
              (* TODO? return just x.name? *)
              let str = OutJ.string_of_deployment_config x in
              Logs.app (fun m ->
                  m "%s Your deployment info is %s" (Logs_.success_tag ()) str)));
      Exit_code.ok
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep whoami`"
            (Logs_.warn_tag ()));
      Exit_code.fatal
