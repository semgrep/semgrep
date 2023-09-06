(* Content of the the ~/.semgrep/settings.yml file.
 * See also Semgrep_envvars.user_settings_file
 *)
type t = {
  has_shown_metrics_notification : bool option;
  api_token : Auth.token option;
  anonymous_user_id : Uuidm.t;
}

(* loading the ~/.semgrep/settings.yml in memory *)
val load : ?maturity:Maturity.t -> unit -> t

(* returns whether the save was successful *)
val save : t -> bool
