open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Load and save the ~/.semgrep/settings.yml file *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* TODO: use ATD to specify the settings file format *)
type t = {
  (* a banner we want to show just once to the user *)
  has_shown_metrics_notification : bool option;
  (* Can be set by SEMGREP_APP_TOKEN *)
  api_token : Auth.token option;
  anonymous_user_id : Uuidm.t;
}

let default =
  let rand = Stdlib.Random.State.make_self_init () in
  {
    has_shown_metrics_notification = None;
    api_token = None;
    anonymous_user_id = Uuidm.v4_gen rand ();
  }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: we should just use ATD to automatically read the settings.yml
   file by converting it first to json and then use ATDgen API *)
let of_yaml = function
  | `O data ->
      let has_shown_metrics_notification =
        List.assoc_opt "has_shown_metrics_notification" data
      and api_token = List.assoc_opt "api_token" data
      and anonymous_user_id = List.assoc_opt "anonymous_user_id" data in
      let/ has_shown_metrics_notification =
        Option.fold ~none:(Ok None)
          ~some:(function
            | `Bool b -> Ok (Some b)
            | _else -> Error (`Msg "has shown metrics notification not a bool"))
          has_shown_metrics_notification
      in
      let/ api_token =
        Option.fold ~none:(Ok None)
          ~some:(function
            | `String s ->
                let token = Auth.unsafe_token_of_string s in
                Ok (Some token)
            | _else -> Error (`Msg "api token not a string"))
          api_token
      in
      let/ anonymous_user_id =
        Option.fold
          ~none:(Error (`Msg "no anonymous user id"))
          ~some:(function
            | `String s ->
                Option.to_result ~none:(`Msg "not a valid UUID")
                  (Uuidm.of_string s)
            | _else -> Error (`Msg "anonymous user id is not a string"))
          anonymous_user_id
      in
      Ok { has_shown_metrics_notification; api_token; anonymous_user_id }
  | _else -> Error (`Msg "YAML not an object")

let to_yaml { has_shown_metrics_notification; api_token; anonymous_user_id } =
  `O
    ((match has_shown_metrics_notification with
     | None -> []
     | Some v -> [ ("has_shown_metrics_notification", `Bool v) ])
    @ (match api_token with
      | None -> []
      | Some v -> [ ("api_token", `String (Auth.string_of_token v)) ])
    @ [ ("anonymous_user_id", `String (Uuidm.to_string anonymous_user_id)) ])

let set_api_token_from_env settings =
  Logs.info (fun m -> m "Checking for API token in environment variables");
  match !Semgrep_envvars.v.app_token with
  (* Check if the token is well formed here since it makes testing much easier,
     as ocaml cannot unset environment variables *)
  | Some token when Auth.well_formed token ->
      Logs.info (fun m ->
          (* SECURITY: Don't log authentication tokens! *)
          m "Found API token in environment variables");
      { settings with api_token = Some token }
  | Some _ ->
      Logs.info (fun m ->
          m
            "Environment variable SEMGREP_APP_TOKEN is set but not \
             well-formed, ignoring it");
      settings
  | None ->
      Logs.info (fun m -> m "No API token found in environment variables");
      settings

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
   Return 'None' if the settings file can't be loaded. The 'settings'
   record will still need to be updated with the API token from the environment
  variable (see 'set_api_token_from_env')
*)
let from_file ?(maturity = Maturity.Default) () =
  let settings = !Semgrep_envvars.v.user_settings_file in
  Logs.info (fun m -> m "Loading settings from %s" !!settings);
  try
    if
      Sys.file_exists !!settings
      && Unix.(stat !!settings).st_kind =*= Unix.S_REG
    then
      let data = UFile.read_file settings in
      let settings_result = Result.bind (Yaml.of_string data) of_yaml in
      match settings_result with
      | Error (`Msg msg) ->
          Logs.warn (fun m ->
              m
                "Bad settings format; %s will be overriden. Contents:\n\
                 %s\n\
                 Decode error: %s" !!settings data msg);
          None
      | Ok settings -> Some settings
    else
      let log =
        match maturity with
        | Maturity.Develop -> Logs.warn
        | Default
        | Legacy
        | Experimental ->
            (* We used to log nothing here. Why not log the same message
               at all maturity levels? Either way, we should always log
               error messages somewhere, never ignore them. *)
            Logs.info
      in
      log (fun m ->
          m "Settings file '%s' does not exist or is not a regular file"
            !!settings);
      None
  with
  (* TODO: Explain When can this happen *)
  | Failure msg ->
      (* TODO: Should be Logs.err? *)
      Logs.info (fun m ->
          m "Failed to load settings from %s: %s" !!settings msg);
      None

(* Try loading settings from the file and/or environment.
 * coupling: cli/src/semgrep/settings.py get_default_contents()
 * You may ask WTF is this behavior. In pysemgrep we:
 * 1. Generate default settings, but set the api token from the envrionment if
 *    it exists.
 * 2. Check if the settings file exists, if not return the default settings
 * 3. If the settings file exists, but the api token is not set in the settings
 *    file, set it from the environment.
 * 4. If the settings file exists and the api token is set, return the settings
 *    file
 * This happens due to some weird unpacking python semantics :/
 * This is a bit convoluted, but we need to keep the same behavior as pysemgrep
 * to avoid breaking changes.
 *)
let load ?maturity ?(include_env = true) () =
  (* Step 1.
   * See cli/src/semgrep/settings.py `generate_default_settings`
   *)
  let default_settings =
    if include_env then set_api_token_from_env default else default
  in
  (* Step 2. *)
  match from_file ?maturity () with
  (* Step 2. iff settings file doesn't exist *)
  | None ->
      Logs.info (fun m -> m "No settings file found, using default settings");
      default_settings
  (* Step 3. *)
  | Some ({ api_token = None; _ } as settings) when include_env ->
      Logs.info (fun m ->
          m
            "Settings file found, but API token is not set in file, pulling \
             from environment variables");
      set_api_token_from_env settings
  (* Step 4. *)
  | Some settings ->
      Logs.info (fun m -> m "Settings file found, using settings from file");
      settings

let save setting =
  let settings = !Semgrep_envvars.v.user_settings_file in
  let yaml = to_yaml setting in
  let str = Yaml.to_string_exn yaml in
  try
    let dir = Fpath.parent settings in
    if not (Sys.file_exists !!dir) then Sys.mkdir !!dir 0o755;
    (* TODO: we don't use UTmp.new_temp_file because this function modifies
     * a global (_temp_files_created) which is then used to autoamtically
     * remove temp files when the process terminates, but in this case the tmp
     * file already disappeared because it was renamed.
     *)
    (* nosemgrep: forbid-tmp *)
    let tmp = Filename.temp_file ~temp_dir:!!dir "settings" "yml" in
    if Sys.file_exists tmp then Sys.remove tmp;
    UFile.write_file (Fpath.v tmp) str;
    (* Create a temporary file and rename to have a consistent settings file,
       even if the power fails (or a Ctrl-C happens) during the write_file. *)
    Unix.rename tmp !!settings;
    Logs.info (fun m -> m "Saved the settings in %s" !!settings);
    true
  with
  | Sys_error e ->
      Logs.warn (fun m ->
          m "Could not write settings file at %a: %s" Fpath.pp settings e);
      false
