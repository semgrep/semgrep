(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Semgrep_metrics.atd to manipulate
   semgrep metrics data to send to metrics.semgrep.dev.

    """
    To prevent sending unintended metrics:
    1. send all data into this class with add_* methods
    2. ensure all add_* methods only set sanitized data

    These methods go directly from raw data to transported data,
    thereby skipping a "stored data" step,
    and enforcing that we sanitize before saving, not before sending.
    """

   Translated from metrics.py (with some parts in scan.py)
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let _metrics_endpoint = "https://metrics.semgrep.dev"

(*
     Configures metrics upload.

     ON - Metrics always sent
     OFF - Metrics never sent
     AUTO - Metrics only sent if config is pulled from the server
  python: was in an intermediate MetricsState before.
*)
type config = On | Off | Auto [@@deriving show]

(* For Cmdliner
 * TOPORT? use lowercase_ascii before? accept ON/OFF/AUTO?
 * TOPORT? Support setting via old environment variable values 0/1/true/false
 * was in scan.py before.
 *)
let converter = Cmdliner.Arg.enum [ ("on", On); ("off", Off); ("auto", Auto) ]

type t = {
  mutable is_using_registry : bool;
  mutable payload : Semgrep_metrics_t.payload;
  config : config;
}

let default_payload =
  {
    Semgrep_metrics_t.event_id = "";
    anonymous_user_id = "";
    started_at = "";
    sent_at = "";
    environment =
      {
        version = "0";
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
  { is_using_registry = false; payload = default_payload; config = Off }

(*****************************************************************************)
(* Global! *)
(*****************************************************************************)

(* It is ugly to have to use a global for the metrics data, but it is
 * configured in the subcommands, modified at a few places,
 * and finally accessed in CLI.safe_run() which makes it hard to pass
 * it around.
 *)
let g = ref default

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let configure config =
  g := { !g with config };
  ()
