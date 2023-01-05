(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
    """
    To prevent sending unintended metrics:
    1. send all data into this class with add_* methods
    2. ensure all add_* methods only set sanitized data

    These methods go directly from raw data to transported data,
    thereby skipping a "stored data" step,
    and enforcing that we sanitize before saving, not before sending.
    """

   Translated from metrics.py
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
type state = On | Off | Auto [@@deriving show]

(* For Cmdliner *)
(* TOPORT? use lowercase_ascii before? accept ON/OFF/AUTO?
   TOPORT? Support setting via old environment variable values 0/1/true/false
*)
let converter = Cmdliner.Arg.enum [ ("on", On); ("off", Off); ("auto", Auto) ]

type _sha256hash = Sha256hash of string

type t = {
  mutable is_using_registry : bool;
  mutable payload : unit;
  state : state;
}

let default = { is_using_registry = false; payload = (); state = Off }

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
let configure state =
  g := { !g with state };
  ()
