module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

type identity_kind = Identity | Deployment

let print (caps : < Cap.network ; Cap.stdout >) (kind : identity_kind) :
    Exit_code.t =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | Some token ->
      let caps = Auth.cap_token_and_network token caps in
      (match kind with
      | Identity ->
          let id = Lwt_platform.run (Semgrep_App.get_identity_async caps) in
          Logs.app (fun m ->
              m "%s You are logged in as %s" (Console.success_tag ()) id)
      | Deployment -> (
          let (x : OutJ.deployment_config option) =
            Lwt_platform.run (Semgrep_App.get_deployment_from_token_async caps)
          in
          match x with
          | None -> failwith "no deployment_config"
          | Some x ->
              (* TODO? return just x.name? *)
              let str = OutJ.string_of_deployment_config x in
              Logs.app (fun m ->
                  m "%s Your deployment info is %s" (Console.success_tag ()) str)
          ));
      Exit_code.ok ~__LOC__
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep whoami`"
            (Console.warning_tag ()));
      Exit_code.fatal ~__LOC__
