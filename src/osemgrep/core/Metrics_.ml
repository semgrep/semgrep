open Common
module Out = Semgrep_output_v1_t
open Semgrep_metrics_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Small wrapper around semgrep-interfaces/semgrep_metrics.atd to prepare
   semgrep metrics data to send to https://metrics.semgrep.dev

   Partially translated from metrics.py

   To date, we have ported the following features from pysemgrep:
     - base payload structure
     - required timing (started_at, sent_at)
     - required event (event_id, anonymous_user_id)
     - basic environment (version, ci, isAuthenticated, integrationName)
     - basic feature tags (subcommands, language)
     - user agent information (version, subcommand)
     - language information (language, numRules, numTargets, totalBytesScanned)

   TODO:
    - add_registry_url
    - parsing stat (parse rates)
    - rule profiling stats
    - cli-envvar? cli-flag? cli-prompt?

    Sending the metrics is handled from the main CLI entrypoint following the
    execution of the CLI.safe_run() function to report the exit code.

    Metrics Flow:
      1. init() - set started_at, event_id, anonymous_user_id
      2. add_feature - tag subcommand, CLI flags, language, etc.
      3. add_user_agent_tag - add CLI version, subcommand, etc.
      4. add_* methods - any other data, or access directly g.payload
      5. prepare_to_send() - set sent_at
      6. string_of_metrics() - serialize metrics payload as JSON string
      7. send_metrics() - send payload to our endpoint
         https://metrics.semgrep.dev (can be changed via SEMGREP_METRICS_URL
         for testing purpose)

    "Life of a Metric Payload" after sending:
      -> API Gateway (Name=Telemetry)
      -> Lambda (Name=SemgrepMetricsGatewayToKinesisIntegration)
      -> Kinesis Stream (Name=semgrep-cli-telemetry)
        |-> S3 Bucket (Name=semgrep-cli-metrics)
          -> Snowflake (SEMGREP_CLI_TELEMETRY)
            -> Metabase (SEMGREP CLI - SNOWFLAKE)
        |-> OpenSearch (Name=semgrep-metrics)

    Notes:
      - Raw payload is ingested by our metrics endpoint exposed via our API
        Gateway
      - We parse the payload and add additional metadata (i.e. sender ip
        address) in our Lambda function
      - We pass the transformed payload to our AWS Kinesis stream
        ("semgrep-cli-telemetry")
      - The payload can be viewed in our internal AWS console (if you can
        guess the shard ID?). The shard ID is based on the Partition Key
        (which is set to the ip address).
        TODO: if someone can figure out how to determine the shard ID easily
        please update this comment!!!
        In practice, your shard ID only needs to found once through trial and
        error by sending multiple payloads until you find a match. There is
        probably a better way to do this...
        I found the following StackOverflow link helpful, but not enough to
        automate this process:
        https://stackoverflow.com/questions/31893297/how-to-determine-shard-id-for-a-specific-partition-key-with-kcl
      - The data viewer URL will look something like https://us-west-2.console.aws.amazon.com/kinesis/home?region=us-west-2#/streams/details/semgrep-cli-telemetry/dataViewer
        where each row is a payload with the IP address as the Partition Key
      - The data is then stored in our S3 bucket ("semgrep-cli-metrics") and
        can be queried via Snowflake or Metabase
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* TODO: set to the cannonical "metrics.semgrep.dev" once we upgrade the
 * host from TLS 1.2 to TLS 1.3
 *)
let metrics_url =
  Uri.of_string "https://oeyc6oyp4f.execute-api.us-west-2.amazonaws.com/Prod/"

(*
     Configures metrics upload.

     ON - Metrics always sent
     OFF - Metrics never sent
     AUTO - Metrics only sent if config is pulled from the server

  python: was in an intermediate MetricsState before.
  TODO? move in a separate Metrics_config.t instead? or rename to 'upload'?
*)
type config = On | Off | Auto [@@deriving show]

(* For Cmdliner
 * TOPORT? use lowercase_ascii before? accept ON/OFF/AUTO?
 * TOPORT? Support setting via old environment variable values 0/1/true/false
 * was in scan.py before.
 *)
let converter = Cmdliner.Arg.enum [ ("on", On); ("off", Off); ("auto", Auto) ]

type t = {
  mutable config : config;
  mutable is_using_registry : bool;
  mutable user_agent : string list;
  mutable payload : Semgrep_metrics_t.payload;
}

let default_payload =
  {
    Semgrep_metrics_t.event_id = "";
    anonymous_user_id = "";
    started_at = "";
    sent_at = "";
    environment =
      {
        version = Version.version;
        projectHash = None;
        configNamesHash = "";
        rulesHash = None;
        ci = None;
        isAuthenticated = false;
        integrationName = None;
      };
    performance =
      {
        numRules = None;
        numTargets = None;
        totalBytesScanned = None;
        fileStats = None;
        ruleStats = None;
        profilingTimes = None;
        maxMemoryBytes = None;
      };
    errors = { returnCode = None; errors = None };
    value =
      {
        features = [];
        numFindings = None;
        numIgnored = None;
        ruleHashesWithFindings = None;
        engineRequested = "OSS";
      };
    parse_rate = [];
    extension =
      {
        machineId = None;
        isNewAppInstall = None;
        sessionId = None;
        version = None;
        ty = None;
      };
  }

let default =
  {
    config = Off;
    is_using_registry = false;
    user_agent = [ spf "Semgrep/%s" Version.version ];
    payload = default_payload;
  }

(*****************************************************************************)
(* Global *)
(*****************************************************************************)

(* It looks ugly to use a global for the metrics data, but it is
 * configured in the subcommands, modified at a few places,
 * and finally accessed in CLI.safe_run() which makes it hard to pass
 * it around.
 * Note that we're not using a ref below, but this must still be viewed
 * as a global because all fields in Metrics_.t and the payload type
 * are mutable.
 *)
let g = default

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let string_of_gmtime (tm : Unix.tm) : string =
  spf "%04d-%02d-%02dT%02d:%02d:%02d+00:00" (1900 + tm.tm_year) (1 + tm.tm_mon)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(* ugly: would be better to have a proper time type in semgrep_metrics.atd *)
let now () : string = string_of_gmtime (Unix.gmtime (Unix.gettimeofday ()))

(*****************************************************************************)
(* Metrics config *)
(*****************************************************************************)
let configure config = g.config <- config
let is_enabled () = g.config <> Off

(*****************************************************************************)
(* User agent *)
(*****************************************************************************)

let add_user_agent_tag (str : string) =
  let str =
    str
    |> Base.String.chop_prefix_if_exists ~prefix:"("
    |> Base.String.chop_suffix_if_exists ~suffix:")"
    |> String.trim |> spf "(%s)"
  in
  g.user_agent <- g.user_agent @ [ str ]

let string_of_user_agent () = String.concat " " g.user_agent

(*****************************************************************************)
(* Payload management *)
(*****************************************************************************)

(* we pass an anonymous_user_id here to avoid a dependency cycle with
 * ../configuring/Semgrep_settings.ml
 *)
let init ~anonymous_user_id ~ci =
  g.payload.started_at <- now ();
  g.payload.event_id <- Uuidm.to_string (Uuidm.v4_gen (Random.get_state ()) ());
  g.payload.anonymous_user_id <- Uuidm.to_string anonymous_user_id;
  (* TODO: this field in semgrep_metrics.atd should be a boolean *)
  if ci then g.payload.environment.ci <- Some "true"

let prepare_to_send () = g.payload.sent_at <- now ()
let string_of_metrics () = Semgrep_metrics_j.string_of_payload g.payload

(*****************************************************************************)
(* add_xxx wrappers *)
(*****************************************************************************)

let add_engine_kind (kind : Out.engine_kind) =
  (* TODO: use a better type in semgrep_metrics.atd for this field *)
  g.payload.value.engineRequested <- Out.show_engine_kind kind

(* TODO? should pass Uri.t directly *)
let add_project_url_hash (project_url : string) =
  let parsed_url = Uri.of_string project_url in
  let sanitized_url =
    match Uri.scheme parsed_url with
    | Some "https" ->
        (* XXX(dinosaure): remove username/password from [parsed_url]. *)
        Uri.make ~scheme:"https" ?host:(Uri.host parsed_url)
          ~path:(Uri.path parsed_url) ()
    | __else__ -> parsed_url
  in
  g.payload.environment.projectHash <-
    Some Digestif.SHA256.(to_hex (digest_string (Uri.to_string sanitized_url)))

let add_configs_hash configs =
  let ctx =
    List.fold_left
      (fun ctx str -> Digestif.SHA256.feed_string ctx str)
      Digestif.SHA256.empty configs
  in
  g.payload.environment.configNamesHash <- Digestif.SHA256.(to_hex (get ctx))

let add_rules_hashes_and_rules_profiling ?profiling:_TODO rules =
  let hashes =
    rules
    |> Common.map Rule.sha256_of_rule
    |> Common.map Digestif.SHA256.to_hex
    |> List.sort String.compare
  in
  let rulesHash_value =
    hashes
    |> List.fold_left
         (fun ctx str -> Digestif.SHA256.feed_string ctx str)
         Digestif.SHA256.empty
  in
  g.payload.environment.rulesHash <-
    Some Digestif.SHA256.(to_hex (get rulesHash_value));
  g.payload.performance.numRules <- Some (List.length rules);
  let ruleStats_value =
    Common.mapi
      (fun idx _rule ->
        { ruleHash = List.nth hashes idx; bytesScanned = 0; matchTime = None })
      rules
  in
  g.payload.performance.ruleStats <- Some ruleStats_value

let add_max_memory_bytes (profiling_data : Report.final_profiling option) =
  Option.iter
    (fun { Report.max_memory_bytes; _ } ->
      g.payload.performance.maxMemoryBytes <- Some max_memory_bytes)
    profiling_data

let add_findings (filtered_matches : (Rule.t * int) list) =
  let ruleHashesWithFindings_value =
    filtered_matches
    |> Common.map (fun (rule, rule_matches) ->
           (Digestif.SHA256.to_hex (Rule.sha256_of_rule rule), rule_matches))
  in
  g.payload.value.ruleHashesWithFindings <- Some ruleHashesWithFindings_value

let add_targets_stats (targets : Fpath.t Set_.t)
    (prof_opt : Report.final_profiling option) =
  let targets = Set_.elements targets in
  let (hprof : (Fpath.t, Report.file_profiling) Hashtbl.t) =
    match prof_opt with
    | None -> Hashtbl.create 0
    | Some prof ->
        prof.file_times
        |> Common.map (fun ({ Report.file; _ } as file_prof) ->
               (Fpath.v file, file_prof))
        |> Common.hash_of_list
  in
  let file_stats =
    targets
    |> Common.map (fun path ->
           let runTime, parseTime, matchTime =
             match Hashtbl.find_opt hprof path with
             | Some fprof ->
                 ( Some fprof.run_time,
                   Some
                     (fprof.rule_times
                     |> Common.map (fun rt -> rt.Report.parse_time)
                     |> Common2.sum_float),
                   Some
                     (fprof.rule_times
                     |> Common.map (fun rt -> rt.Report.match_time)
                     |> Common2.sum_float) )
             | None -> (None, None, None)
           in
           {
             size = File.filesize path;
             numTimesScanned =
               (match Hashtbl.find_opt hprof path with
               | None -> 0
               | Some fprof -> List.length fprof.rule_times);
             parseTime;
             matchTime;
             runTime;
           })
  in
  g.payload.performance.fileStats <- Some file_stats;
  g.payload.performance.totalBytesScanned <-
    Some (targets |> Common.map File.filesize |> Common2.sum_int);
  g.payload.performance.numTargets <- Some (List.length targets)

let add_errors errors =
  g.payload.errors.errors <-
    Some
      (errors
      |> Common.map (fun (err : Out.cli_error) -> (* TODO? enough? *)
                                                  err.type_))

let add_profiling profiler =
  g.payload.performance.profilingTimes <- Some (Profiler.dump profiler)

let add_token token =
  g.payload.environment.isAuthenticated <- Option.is_some token

let add_exit_code code =
  let code = Exit_code.to_int code in
  g.payload.errors.returnCode <- Some code

let add_feature ~category ~name =
  let str = Format.asprintf "%s/%s" category name in
  g.payload.value.features <- str :: g.payload.value.features
