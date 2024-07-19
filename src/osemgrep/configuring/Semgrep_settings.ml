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
  api_token : Auth.token option;
  anonymous_user_id : Uuidm.t;
}

let default_settings =
  {
    has_shown_metrics_notification = None;
    api_token = None;
    anonymous_user_id = Uuidm.v `V4;
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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let set_api_token ~include_env settings =
  if include_env then
    match !Semgrep_envvars.v.app_token with
    | Some token -> { settings with api_token = Some token }
    | None -> settings
  else settings

(*
   Return 'None' if the settings file can't be loaded. The 'settings'
   record need to be completed with the 'set_api_token'.
*)
let load_opt ?(maturity = Maturity.Default) ?(include_env = true) () =
  let settings = !Semgrep_envvars.v.user_settings_file in
  Logs.info (fun m -> m "Loading settings from %s" !!settings);
  try
    if
      Sys.file_exists !!settings
      && Unix.(stat !!settings).st_kind =*= Unix.S_REG
    then
      let data = UFile.read_file settings in
      let opt_settings =
        match Yaml.of_string data with
        | Error (`Msg msg) ->
            Logs.warn (fun m ->
                m
                  "Bad settings format; %s will be overriden. Contents:\n\
                   %s\n\
                   Decode error: %s" !!settings data msg);
            None
        | Ok value -> (
            match of_yaml value with
            | Error (`Msg msg) ->
                Logs.warn (fun m ->
                    m
                      "Bad settings format; %s will be overriden. Contents:\n\
                       %s\n\
                       Decode error: %s" !!settings data msg);
                None
            | Ok settings -> Some settings)
      in
      (* TODO: explain why we set the api_token field from the environment
         variable only in the case of some failures but not in case of
         an exception. *)
      let settings =
        match opt_settings with
        | None -> default_settings
        | Some settings -> settings
      in
      Some (set_api_token ~include_env settings)
    else (
      (match maturity with
      | Maturity.Develop ->
          Logs.warn (fun m ->
              m "Settings file '%s' does not exist or is not a regular file"
                !!settings)
      | Default
      | Legacy
      | Experimental ->
          (* We used to log nothing here. Why not log the same message
             at all maturity levels? Either way, we should always log
             error messages somewhere, never ignore them. *)
          Logs.info (fun m ->
              m "Settings file '%s' does not exist or is not a regular file"
                !!settings));
      None)
  with
  | Failure msg ->
      Logs.info (fun m ->
          m "Failed to load settings from %s: %s" !!settings msg);
      None

let load ?maturity ?include_env () =
  match load_opt ?maturity ?include_env () with
  | None -> default_settings
  | Some settings -> settings

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
