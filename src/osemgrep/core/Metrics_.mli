(*
   Configures metrics upload.

   ON - Metrics always sent
   OFF - Metrics never sent
   AUTO - Metrics only sent if config is pulled from the server
*)
type config = On | Off | Auto [@@deriving show]

val is_enabled : config -> bool
(** [is_enabled config] returns [true] if the given configuration enables metrics.
    Otherwise, it returns [false]. *)

(* For Cmdliner *)
val converter : config Cmdliner.Arg.conv

type t = {
  mutable is_using_registry : bool;
  mutable payload : Semgrep_metrics_t.payload;
  mutable config : config;
}

val default : t

(* global you should not access directly *)
val g : t

(* initialize g that is then modified by the add_xxx functions
 * below and finally accessed in send() further below.
 *)
val configure : config -> unit
val add_engine_type : name:string -> unit
val is_using_registry : unit -> bool
val add_project_url : string option -> unit
val add_configs : configs:string list -> unit
val add_integration_name : string option -> unit
val add_rules : ?profiling:Semgrep_output_v1_t.core_timing -> Rule.rules -> unit
val add_max_memory_bytes : Report.final_profiling option -> unit
val add_findings : (Rule.t * int) list -> unit
val add_targets : Fpath.t Set_.t -> Report.final_profiling option -> unit
val add_errors : Semgrep_output_v1_t.core_error list -> unit
val add_profiling : Profiler.t -> unit
val add_token : 'a option -> unit
val add_version : string -> unit
val add_exit_code : Exit_code.t -> unit
val add_feature : category:string -> name:string -> unit
