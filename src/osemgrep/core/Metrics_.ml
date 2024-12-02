open Common
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Small wrapper around semgrep-interfaces/semgrep_metrics.atd to prepare
   the metrics data to send to https://metrics.semgrep.dev

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
    - rule profiling stats (including ruleStats)
    - cli-envvar? cli-prompt?
    - more?

    Sending the metrics is handled from the main CLI entrypoint following the
    execution of the CLI.safe_run() function to report the exit code.

    Metrics flow in osemgrep:
      1. init() (in CLI.ml) - set started_at, event_id, anonymous_user_id
      2. configure() (in the Xxx_subcommand.ml) to enable/disable metrics
      3. add_feature - tag subcommand, CLI flags, language, etc.
      4. add_user_agent_tag - add CLI version, subcommand, etc.
      5. add_* methods - any other data, or access directly g.payload
      6. prepare_to_send() - set sent_at
      7. string_of_metrics() - serialize metrics payload as JSON string
      8. send_metrics() (in CLI.ml) - send payload to our endpoint
         https://metrics.semgrep.dev (can be changed via SEMGREP_METRICS_URL
         for testing purpose)

    Metrics flow outside (o)semgrep: See
    https://www.notion.so/semgrep/Life-of-a-Semgrep-CLI-metrics-payload-8b6442c4ce164819aa55bab08d83c1f6
    but basically after posting to metrics.semgrep.dev:
      -> API Gateway (Name=Telemetry)
      -> Lambda (Name=SemgrepMetricsGatewayToKinesisIntegration)
         see semgrep-app-lambdas/metrics-handler/prod/index.js
      -> Kinesis Stream (Name=semgrep-cli-telemetry)
        |-> S3 Bucket (Name=semgrep-cli-metrics)
          -> Snowflake (SEMGREP_CLI_TELEMETRY)
            -> Metabase (SEMGREP CLI - SNOWFLAKE)
        |-> OpenSearch (Name=semgrep-metrics) (TODO: where??)

    For metabase, you can watch "Metabase for PA engineers" talk by Emma here:
    https://drive.google.com/file/d/1BJNR578M3KxbuuIU5xNPFkhbYccfo9XH/view
    You can see the data here:
    https://metabase.corp.semgrep.dev/browse/databases/6-semgrep-cli-snowflake
    and especially the CLI Run table and inside the EVENT column which contains
    the whole JSON payload.
    However it does not seem to work very well.

    For Snowflake, which seems more responsive and with less errors, try
    https://app.snowflake.com/fbwpxcx/xx83553/#/data/databases/SEMGREP_CLI
    and in https://app.snowflake.com/fbwpxcx/xx83553/worksheets click '+' to
    write query, such as

       SELECT *
       FROM "SEMGREP_CLI"."PUBLIC"."CLI_RUN"
       WHERE date_trunc('day', CAST(INGESTED_AT AS TIMESTAMP)) >=
             date_trunc('day', current_timestamp - INTERVAL '90 days')
          AND USER_AGENT LIKE '%logout%'

    Notes:
      - We parse the payload and add additional metadata (i.e., sender IP, the
        user Agent) in our Lambda function
        (see semgrep-app-lambdas/metrics-handler/prod/index.js).
        We do not parse the payload in a typed way, we access JSON fields
        directly (bad)
      - We pass the transformed payload to our AWS Kinesis stream
        ("semgrep-cli-telemetry")
      - The payload can be viewed in our internal AWS console (if you can
        guess the shard ID?). The shard ID is based on the Partition Key
        (which is set to the ip address).
        TODO: if someone can figure out how to determine the shard ID easily
        please update this comment.
        In practice, your shard ID only needs to found once through trial and
        error by sending multiple payloads until you find a match. There is
        probably a better way to do this.
        I found the following StackOverflow link helpful, but not enough to
        automate this process:
        https://stackoverflow.com/questions/31893297/how-to-determine-shard-id-for-a-specific-partition-key-with-kcl
      - The data viewer URL will look something like https://us-west-2.console.aws.amazon.com/kinesis/home?region=us-west-2#/streams/details/semgrep-cli-telemetry/dataViewer
        where each row is a payload with the IP address as the Partition Key
      - The data is then stored in our S3 bucket ("semgrep-cli-metrics") and
        can be queried via Snowflake or Metabase such as
        https://metabase.corp.semgrep.dev/browse/databases/6-semgrep-cli-snowflake
        https://app.snowflake.com/fbwpxcx/xx83553/#/data/databases/SEMGREP_CLI

    alt: this file should be called simply Metrics.ml but this would conflict
    with a module using the same name in one of the OCaml library we use.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let metrics_url : Uri.t = Uri.of_string "https://metrics.semgrep.dev"

(*
     Configures metrics upload.

     On   - Metrics always sent
     Off  - Metrics never sent
     Auto - Metrics only sent if config is pulled from the registry
            or if using the Semgrep App.

  What is the rational for Auto? I guess if the user is requesting rules
  from our Registry or App, we already can identify him by his IP
  so we might as well get more data from him?

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
  (* works with Auto *)
  mutable is_using_registry : bool;
  (* TODO: not fully set for now; should set in CI and more contexts *)
  mutable is_using_app : bool;
  mutable user_agent : string list;
  mutable payload : Semgrep_metrics_t.payload;
}

let now () : Timedesc.Timestamp.t = Timedesc.Timestamp.now ()

let default_payload =
  let rand = Stdlib.Random.State.make_self_init () in
  {
    Semgrep_metrics_t.event_id = Uuidm.v4_gen rand ();
    anonymous_user_id = "";
    started_at = now ();
    sent_at = now ();
    environment =
      {
        version = Version.version;
        os = Sys.os_type;
        isTranspiledJS = false;
        projectHash = None;
        configNamesHash = Digestif.SHA256.digest_string "<noconfigyet>";
        rulesHash = None;
        ci = None;
        isDiffScan = false;
        isAuthenticated = false;
        integrationName = None;
        deployment_id = None;
      };
    performance =
      {
        numRules = None;
        numTargets = None;
        totalBytesScanned = None;
        fileStats = None;
        (* ugly: this should be None, but some code in the semgrep-app-lambdas
         * repo in metrics-handler/prod/index.js assumes a Some here.
         *)
        ruleStats = Some [];
        profilingTimes = None;
        maxMemoryBytes = None;
      };
    errors = { returnCode = None; errors = None };
    value =
      {
        features = [];
        (* TODO: proFeatures *)
        proFeatures = None;
        (* TODO: numFindings *)
        numFindings = None;
        (* TODO: numFindingsByProduct *)
        numFindingsByProduct = None;
        numIgnored = None;
        ruleHashesWithFindings = None;
        engineRequested = "OSS";
        engineConfig = None;
        interfileLanguagesUsed = Some [];
      };
    parse_rate = [];
    extension =
      {
        machineId = None;
        isNewAppInstall = None;
        sessionId = None;
        version = None;
        ty = None;
        autofixCount = None;
        ignoreCount = None;
      };
  }

let default =
  {
    (* default to Off, so don't forget to call Metrics_.configure()
     * to change it in the different subcommands.
     *)
    config = Off;
    (* should be set in Rule_fetching.ml when using the Registry or App *)
    is_using_registry = false;
    is_using_app = false;
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
(* Metrics config *)
(*****************************************************************************)
let configure config = g.config <- config

let is_enabled () =
  match g.config with
  | Off -> false
  | On -> true
  | Auto ->
      (* TOPORT:
         # When running logged in with `semgrep ci`, configs are
         # resolved before `self.is_using_registry` is set.
         # However, these scans are still pulling from the registry
         # TODO?
         ## using_app = (
         ##    state.command.get_subcommand() == "ci"
         ##    and state.app_session.is_authenticated
         ## )
      *)
      g.is_using_registry || g.is_using_app

(*****************************************************************************)
(* User agent *)
(*****************************************************************************)

(* The user_agent is not part of the payload we send to the metrics
 * endpoint, but it's part of the HTTP request and an AWS Lambda in the metrics
 * pipeline actually adds it back to the payload.
 *
 * This function is used to add extra "tags" to the agent around
 * parenthesis (e.g., "(Docker)", "(osemgrep)", "(command/login)")
 *)
let add_user_agent_tag (str : string) =
  let str =
    str
    (* TODO: don't use JaneStreet Base until we agree to do so *)
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
let init (caps : < Cap.random ; .. >) ~anonymous_user_id ~ci =
  g.payload.started_at <- now ();
  g.payload.event_id <- Uuidm.v4_gen (CapRandom.get_state caps#random ()) ();
  g.payload.anonymous_user_id <- Uuidm.to_string anonymous_user_id;
  (* TODO: this field in semgrep_metrics.atd should be a boolean *)
  if ci then g.payload.environment.ci <- Some "true"

let prepare_to_send () = g.payload.sent_at <- now ()
let string_of_metrics () = Semgrep_metrics_j.string_of_payload g.payload

(*****************************************************************************)
(* add_xxx wrappers *)
(*****************************************************************************)
let add_engine_type (engine_type : Engine_type.t) =
  let metrics_from_engine_type et : Semgrep_metrics_t.engine_config =
    match (et : Engine_type.t) with
    | OSS ->
        {
          analysis_type = `Intraprocedural;
          pro_langs = false;
          code_config = None;
          secrets_config = None;
          supply_chain_config = None;
        }
    | PRO
        {
          analysis;
          extra_languages;
          secrets_config;
          code_config;
          supply_chain_config;
          _;
        } ->
        {
          analysis_type =
            (match analysis with
            | Intraprocedural -> `Intraprocedural
            | Interprocedural -> `Interprocedural
            | Interfile -> `Interfile);
          code_config =
            Option.map
              (fun () : Semgrep_metrics_t.code_config -> { _rfu = None })
              code_config;
          secrets_config =
            Option.map
              (fun (conf : Engine_type.secrets_config) :
                   Semgrep_metrics_t.secrets_config ->
                {
                  permitted_origins =
                    (if conf.allow_all_origins then `Any else `NoCommunity);
                })
              secrets_config;
          supply_chain_config =
            Option.map
              (fun () : Semgrep_metrics_t.supply_chain_config ->
                { _rfu = None })
              supply_chain_config;
          pro_langs = extra_languages;
        }
  in
  (* TODO: remove this field? *)
  g.payload.value.engineRequested <-
    OutJ.show_engine_kind
      (match engine_type with
      | OSS -> `OSS
      | PRO _ -> `PRO);
  g.payload.value.engineConfig <- Some (metrics_from_engine_type engine_type)

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
    Some (Digestif.SHA256.digest_string (Uri.to_string sanitized_url))

let add_configs_hash configs =
  let ctx =
    List.fold_left
      (fun ctx str -> Digestif.SHA256.feed_string ctx str)
      Digestif.SHA256.empty configs
  in
  g.payload.environment.configNamesHash <- Digestif.SHA256.get ctx

let add_rules_hashes_and_rules_profiling ?profiling:_TODO rules =
  let hashes =
    rules
    |> List_.map Rule.sha256_of_rule
    |> List_.map Digestif.SHA256.to_hex
    |> List.sort String.compare
  in
  let rulesHash_value =
    hashes
    |> List.fold_left
         (fun ctx str -> Digestif.SHA256.feed_string ctx str)
         Digestif.SHA256.empty
  in
  g.payload.environment.rulesHash <- Some (Digestif.SHA256.get rulesHash_value);
  g.payload.performance.numRules <- Some (List.length rules);
  (* TODO: Properly populate g.payload.performance.ruleStats.
   *
   * Currently, when we have thousands of rules, they will bloat the
   * metrics payload. Right now in metrics.py, we are only populating
   * these stats when both matching time and bytes scanned are greater
   * than 0.
   *
   * ugly: see the comment above on ruleStats in default_payload why we set this
   * to Some [] instead of None.
   *)
  g.payload.performance.ruleStats <- Some []

let add_max_memory_bytes (profiling_data : Core_profiling.t option) =
  Option.iter
    (fun { Core_profiling.max_memory_bytes; _ } ->
      g.payload.performance.maxMemoryBytes <- Some max_memory_bytes)
    profiling_data

let add_rules_hashes_and_findings_count (filtered_matches : (Rule.t * int) list)
    =
  (* Rules with 0 findings don't carry a lot of information
   * compared to rules that actually have findings. Rules with 0
   * findings also increase the size of the metrics quite
   * significantly, e.g., when the number of rules grows up to
   * magnitudes of 10k. So we filter them out in the metrics.
   *)
  let ruleHashesWithFindings_value =
    filtered_matches
    |> List_.filter_map (fun (rule, rule_matches) ->
           if rule_matches > 0 then
             Some
               (Digestif.SHA256.to_hex (Rule.sha256_of_rule rule), rule_matches)
           else None)
  in
  g.payload.value.ruleHashesWithFindings <- Some ruleHashesWithFindings_value

let add_targets_stats (targets : Fpath.t Set_.t)
    (prof_opt : Core_profiling.t option) =
  let targets = Set_.elements targets in
  let hprof : (Fpath.t, Core_profiling.file_profiling) Hashtbl.t =
    match prof_opt with
    | None -> Hashtbl.create 0
    | Some (prof : Core_profiling.t) ->
        prof.file_times
        |> List_.map (fun ({ Core_profiling.file; _ } as file_prof) ->
               (file, file_prof))
        |> Hashtbl_.hash_of_list
  in

  let file_stats =
    targets
    |> List_.map (fun path ->
           let runTime, parseTime, matchTime =
             match Hashtbl.find_opt hprof path with
             | Some (fprof : Core_profiling.file_profiling) ->
                 ( Some fprof.run_time,
                   Some
                     (fprof.rule_times
                     |> List_.map (fun rt -> rt.Core_profiling.rule_parse_time)
                     |> Common2.sum_float),
                   Some
                     (fprof.rule_times
                     |> List_.map (fun rt -> rt.Core_profiling.rule_match_time)
                     |> Common2.sum_float) )
             | None -> (None, None, None)
           in
           {
             Semgrep_metrics_t.size = UFile.filesize path;
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
    Some (targets |> List_.map UFile.filesize |> Common2.sum_int);
  g.payload.performance.numTargets <- Some (List.length targets)

(* TODO? type_ is enough? or want also to log the path? but too
 * privacy sensitive maybe?
 *)
let string_of_error (err : OutJ.cli_error) : string =
  Error.string_of_error_type err.type_

let add_errors errors =
  g.payload.errors.errors <-
    Some
      (errors |> List_.map (fun (err : OutJ.cli_error) -> string_of_error err))

let add_profiling profiler =
  g.payload.performance.profilingTimes <- Some (Profiler.dump profiler)

let add_exit_code code =
  let code = Exit_code.to_int code in
  g.payload.errors.returnCode <- Some code

(* Covered: "language/xxx", "cli-flag/xxx", "subcommand/xxx"
 * TOPORT: "config/xxx", "ruleset/xxx", "cli-envvar/xxx", "cli-prompt/xxx"
 *  "output/xxx"
 *)
let add_feature ~category ~name =
  let str = Format.asprintf "%s/%s" category name in
  g.payload.value.features <- str :: g.payload.value.features

(*****************************************************************************)
(* Init and Send *)
(*****************************************************************************)
(* The code is now in CLI.ml *)
