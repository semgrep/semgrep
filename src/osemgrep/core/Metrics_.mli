(** Metrics upload configuration *)
type config =
  | On  (** Always send metrics *)
  | Off  (** Never send metrics *)
  | Auto
      (** Send metrics when config is pulled from registry or using the Semgrep Cloud Platform *)
[@@deriving show]

(* For Cmdliner, to process the --metrics=<xxx> flag *)
val converter : config Cmdliner.Arg.conv

(* https://metrics.semgrep.dev *)
val metrics_url : Uri.t

type t = {
  mutable config : config;
  mutable is_using_registry : bool;
  mutable is_using_app : bool;
  mutable user_agent : string list;
      (** The user agent used when sending data to {v https://metrics.semgrep.dev v}.

          We override the default value (e.g. ["ocaml-cohttp/MAJOR.MINOR.PATCH"])
          with a custom string built from:
          - the version of semgrep
          - the subcommand
          - any custom value specified in [SEMGREP_USER_AGENT_APPEND]. For
            example, we set this variable to ["Docker"] when running from a Docker
            container (see Dockerfile) allowing us to measure usage of semgrep
            running from Docker container images that we distribute.

          For example, this field might contain
          [["Semgrep/1.39.0"; "(Docker)"; "(command/scan)"]],
          which when concatenated will look like
          ["Semgrep/1.39.0 (Docker) (command/scan)"].

          alt: we could encode this information in the payload instead. *)
  mutable payload : Semgrep_metrics_t.payload;
      (** The actual metrics payload to send to {v https://metrics.semgrep.dev v}.

          Note that the {!user_agent} itself contains useful metrics. *)
}

(* g stands for global. This is initialized with a default payload,
 * config set to Off and the "Semgrep/<version>" in user_agent.
 *)
val g : t

(* set g.config *)
val configure : config -> unit

(* check whether g.config is On or
 * set to Auto and (is_using_registry or is_using_app) *)
val is_enabled : unit -> bool

(* Add more tags to the user_agent string (e.g., "command/scan").
 * Note that the function automatically wrap the tag with
 * parenthesis around (e.g., "(command/scan)").
 *)
val add_user_agent_tag : string -> unit
val string_of_user_agent : unit -> string

(* initialize the payload in g, which can then be modified by the
 * add_xxx functions below (or by accessing directly g.payload) and
 * finally accessed in string_of_metrics().
 * You should not call this function though; only CLI.metrics_init()
 * should call it.
 *)
val init : < Cap.random ; .. > -> anonymous_user_id:Uuidm.t -> ci:bool -> unit

(* just set the payload.sent_at field *)
val prepare_to_send : unit -> unit

(* serialize the payload to send using ATD generated code *)
val string_of_metrics : unit -> string

(* important metrics *)

(* we just hash the project URL; we do not send the project URL.
 * TODO: should be Uri.t
 *)
val add_project_url_hash : string -> unit

(* again, we just send the hash of the --config used
 * TODO: could also take the parsed form Rules_config.t so can add registry
 * metrics.
 *)
val add_configs_hash : Rules_config.config_string list -> unit

val add_rules_hashes_and_rules_profiling :
  ?profiling:Semgrep_output_v1_t.profile -> Rule.rules -> unit

val add_rules_hashes_and_findings_count : (Rule.t * int) list -> unit
val add_targets_stats : Fpath.t Set_.t -> Core_profiling.t option -> unit
val add_engine_type : Engine_type.t -> unit
val add_exit_code : Exit_code.t -> unit

(* ex: "language/python" *)
val add_feature : category:string -> name:string -> unit

(* profiling data (TODO: should merge the 2 types and 2 calls) *)
val add_profiling : Profiler.t -> unit
val add_max_memory_bytes : Core_profiling.t option -> unit

(* useful for us to improve semgrep *)
val add_errors : Semgrep_output_v1_t.cli_error list -> unit
