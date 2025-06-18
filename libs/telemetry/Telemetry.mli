(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type scope = Opentelemetry.Scope.t [@@deriving show]

type config = {
  endpoint : Uri.t;
  (* Telemetry software like datadog and opentelemetry will organize traces by
     the environment they come from (e.g. development, staging, production). env
     here sets that metadata *)
  env : string option;
  (* To add data to our opentelemetry top scope, so easier to filter *)
  top_level_scope : scope option;
}
[@@deriving show]

type user_data = Opentelemetry.value [@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
module Attributes : sig
  val version : string
  val instance_id : string
  val deployment_environment_name : string
  val vcs_ref_head_revision : string
  val vcs_ref_head_name : string
  val thread_id : string
  val scan_engine : string
  val scan_source : string
  val experiment_name : string
end

(*****************************************************************************)
(* Helper Functions *)
(*****************************************************************************)
val get_current_scope : unit -> scope option
(** Expose the Trace function to get the current scope *)

val get_global_attr_opt : string -> (string * user_data) option
(** [get_global_attr_opt some_attr] will return some key value pair if the key
    is set in the global attributes *)

val find_global_attrs : string list -> (string * user_data) list
(** [find_global_attrs some_attribute_keys] will return a list of global
    resource attributes that are set given the keys passed. If a key is not set
    it will not be included in the list. This is useful for checking what global
    attrs may be set, and applying them to OTel events, such as metrics *)

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_otel : ?attrs:(string * user_data) list -> string -> Uri.t -> unit
(** [configure_otel service_name tracing_endpoint] Before instrumenting
    anything, configure OTel. This should only be run once in a program, because
    it creates a backend with threads, HTTP connections, etc. when called.
    [service_name] is the name of the service. [~attrs] can be used to set
    additional global attributes (such as ["service.version"]), which are tags
    that will be applied to all outgoing traces/metrics/logs etc.

    NOTE: this will set the active trace endpoint to
    whatever is passed. This endpoint will be used when restarting tracing via
    [restart_otel] *)

val stop_otel : unit -> unit
(** [stop_otel ()] explicitly shuts down the Otel
    collector. If tracing has been setup this MUST be called before forking
    (such as in {!Parmap}), or you WILL experience random segfaults. This is
    safe to call multiple times in a row. See [restart_otel] to continue
    tracing after calling this.

    Example:
    {[
      stop_otel ();
      (if Unix.fork () = 0 then
      print_endline "child"
      else
      print_endline "parent");
      restart_otel ();
    ]}
 *)

val restart_otel : unit -> unit
(** [restart_otel ()] will re-setup the Otel backend after [stop_otel] is
    called to continue tracing. This is a no-op if [configure_otel] has not
    been called. Will fail if called multiple times. See {!stop_otel} for an
    example*)

val with_otel_paused : (unit -> 'a) -> 'a
(** [with_otel_paused f] will run [f] with tracing paused. This is usually
    called before forking, as Otel can segfault if it is not paused before
    forking. Essentially this calls [stop_otel] and then
    [restart_otel]. *)
