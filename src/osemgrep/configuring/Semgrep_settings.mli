(* Content of the ~/.semgrep/settings.yml file.
 * See also Semgrep_envvars.user_settings_file
 *)
type t = {
  (* a banner we want to show just once to the user *)
  has_shown_metrics_notification : bool option;
  api_token : Auth.token option;
  anonymous_user_id : Uuidm.t;
}

val default : t
(** Default settings. Exposed for testing *)

(* Load the settings file (default: ~/.semgrep/settings.yml).
 * This function is intended for testing.
 * Return 'None' if the settings file can't be loaded.
 *)
val from_file : ?maturity:Maturity.t -> unit -> t option

(* Load the settings file and additionally read 'api_token'
 * from the 'SEMGREP_APP_TOKEN' environment variable if 'include_env'
 * is true (default). The full spec is actually a bit complicated:
 * 1. Generate default settings, but set the api token from the envrionment if
 *    it exists.
 * 2. Check if the settings file exists, if not return the default settings
 * 3. If the settings file exists, but the api token is not set in the settings
 *    file, set it from the environment.
 * 4. If the settings file exists and the api token is set, return the settings
 *    file
 *)
val load : ?maturity:Maturity.t -> ?include_env:bool -> unit -> t

(* save and returns whether the save was successful *)
val save : t -> bool
