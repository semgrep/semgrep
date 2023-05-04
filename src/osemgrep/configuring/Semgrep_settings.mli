(* Content of the the ~/.semgrep/settings.yml file.
 * See also Semgrep_envvars.user_settings_file
 *)
type t = {
  has_shown_metrics_notification : bool option;
  api_token : string option;
  anonymous_user_id : Uuidm.t;
}

val save : t -> bool
val get : unit -> t
