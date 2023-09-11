(*
   Configures metrics upload.

   On - Metrics always sent
   Off - Metrics never sent
   Auto - Metrics only sent if config is pulled from the server
*)
type config = On | Off | Auto [@@deriving show]

(* For Cmdliner, to process the --metrics=<xxx> flag *)
val converter : config Cmdliner.Arg.conv

(* https://metrics.semgrep.dev *)
val metrics_url : Uri.t

type t = {
  mutable config : config;
  mutable is_using_registry : bool;
  (* The user agent used when sending data to https://metrics.semgrep.dev.
   * We override the default value (e.g. "ocaml-cohttp/5.3.0") with a
   * custom string built from:
   *  - the version of semgrep
   *  - the subcommand
   *  - any custom value specified in $SEMGREP_USER_AGENT_APPEND.
   *    For example, we set this variable to "Docker" when running
   *    from a Docker container (see Dockerfile) allowing us to measure
   *    usage of semgrep running from Docker container images that
   *    we distribute.
   *
   * For example, this field might contain
   *    ["Semgrep/1.39.0"; "(Docker)"; "(command/scan)"]
   * which when concatenated will look like
   *   "Semgrep/1.39.0 (Docker) (command/scan)"
   *
   * alt: we could encode this information in the payload instead.
   *)
  mutable user_agent : string list;
  (* The stuff we send to https://metrics.semgrep.dev
   * Note that the user_agent itself contain useful metrics.
   *)
  mutable payload : Semgrep_metrics_t.payload;
}

(* g stands for global. This is initialized with a default payload,
 * config set to Off and the "Semgrep/<version>" in user_agent.
 *)
val g : t

(* set g.config *)
val configure : config -> unit

(* check whether g.config is On (or Auto) *)
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
 *)
val init : anonymous_user_id:Uuidm.t -> ci:bool -> unit

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
 * TODO: should also take the parsed form, so can add registry metrics.
 *)
val add_configs_hash :
  string (* Semgrep_dashdash_config.config_str*) list -> unit

val add_rules_hashes_and_rules_profiling :
  ?profiling:Semgrep_output_v1_t.core_timing -> Rule.rules -> unit

val add_findings : (Rule.t * int) list -> unit
val add_targets_stats : Fpath.t Set_.t -> Report.final_profiling option -> unit
val add_engine_kind : Semgrep_output_v1_t.engine_kind -> unit
val add_token : 'a option -> unit
val add_exit_code : Exit_code.t -> unit

(* ex: "language/python" *)
val add_feature : category:string -> name:string -> unit

(* profiling data (TODO: should merge the 2 types and 2 calls) *)
val add_profiling : Profiler.t -> unit
val add_max_memory_bytes : Report.final_profiling option -> unit

(* useful for us to improve semgrep *)
val add_errors : Semgrep_output_v1_t.cli_error list -> unit
