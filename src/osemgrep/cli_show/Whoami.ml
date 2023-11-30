module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

type identity_kind = Identity | Deployment | Bucket

let print (kind : identity_kind) : Exit_code.t =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | Some token ->
      (match kind with
      | Bucket ->
          let settings = Semgrep_settings.load () in
          let anonymous_user_id = settings.Semgrep_settings.anonymous_user_id in
          Logs.debug (fun m ->
              m "anonymous_user_id: %s" (anonymous_user_id |> Uuidm.to_string));
          let bytes = anonymous_user_id |> Uuidm.to_bytes |> Bytes.of_string in
          Logs.debug (fun m ->
              m "bytes: %s" (bytes |> Bytes.escaped |> Bytes.to_string));
          let int_repr = Bytes.get_int64_be bytes 0 |> Int64.to_int in
          Logs.debug (fun m -> m "int_repr: %d" int_repr);
          let bucket = int_repr mod 1000 |> abs in
          Logs.app (fun m ->
              m "%s bucket %d" (Logs_helpers.success_tag ()) bucket)
      | Identity ->
          let id = Lwt_platform.run (Semgrep_App.get_identity_async ~token) in
          Logs.app (fun m ->
              m "%s You are logged in as %s" (Logs_helpers.success_tag ()) id)
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
                  m "%s Your deployment info is %s"
                    (Logs_helpers.success_tag ())
                    str)));
      Exit_code.ok
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep whoami`"
            (Logs_helpers.warn_tag ()));
      Exit_code.fatal
