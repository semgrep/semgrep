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
   This function is intended for testing. *)
val from_file : ?maturity:Maturity.t -> unit -> t option

(* Load the settings file and additionally read 'api_token'
   from the 'SEMGREP_APP_TOKEN' environment variable if 'include_env'
   is true (default). *)
val load : ?maturity:Maturity.t -> ?include_env:bool -> unit -> t

(* save and returns whether the save was successful *)
val save : t -> bool
