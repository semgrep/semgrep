(*
   'semgrep scan' subcommand

   Translated from scan.py
*)

open Printf

(* Provide 'Term', 'Arg', and 'Manpage' modules. *)
open Cmdliner

type conf = {
  autofix : bool;
  baseline_commit : string option;
  metrics : Metrics.State.t;
}

(* All the business logic after command-line parsing. *)
let run _conf =
  print_endline "hello";
  0

(*************************************************************************)
(* Various utilities *)
(*************************************************************************)

let _get_cpu_count () =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)

let _validate_lang option lang_str =
  match lang_str with
  | None -> failwith (sprintf "%s and -l/--lang must both be specified" option)
  | Some lang -> lang

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let o_autofix =
  CLI_common.negatable_flag ~options:[ "a"; "autofix" ]
    ~neg_options:[ "no-autofix" ]
    ~doc:
      "Apply autofix patches. WARNING: data loss can occur with this flag. \
       Make sure your files are stored in a version control system. Note that \
       this mode is experimental and not guaranteed to function properly."

let o_baseline_commit =
  let info =
    Arg.info [ "baseline_commit" ]
      ~doc:
        "Only show results that are not found in this commit hash. Aborts run \
         if not currently in a git directory, there are unstaged changes, or \
         given baseline hash doesn't exist"
      ~env:(Cmd.Env.info "SEMGREP_BASELINE_COMMIT")
    (* TODO: support also SEMGREP_BASELINE_REF; unfortunately cmdliner
             supports only one environment variable per option *)
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let o_metrics =
  let info =
    Arg.info [ "metrics" ]
      ~doc:
        "Configures how usage metrics are sent to the Semgrep server. If \
         'auto', metrics are sent whenever the --config value pulls from the \
         Semgrep server. If 'on', metrics are always sent. If 'off', metrics \
         are disabled altogether and not sent. If absent, the \
         SEMGREP_SEND_METRICS environment variable value will be used. If no \
         environment variable, defaults to 'auto'."
      ~env:(Cmd.Env.info "SEMGREP_SEND_METRICS")
  in
  Arg.value (Arg.opt Metrics.State.converter Metrics.State.Auto info)

(*** Subcommand 'scan' ***)

let cmdline_term run =
  let combine autofix baseline_commit metrics =
    run { autofix; baseline_commit; metrics }
  in
  Term.(const combine $ o_autofix $ o_baseline_commit $ o_metrics)

let doc = "run semgrep rules on files"

let man =
  [
    `S Manpage.s_description;
    `P
      "Searches TARGET paths for matches to rules or patterns. Defaults to \
       searching entire current working directory.";
    `P "To get started quickly, run";
    `Pre "semgrep --config auto .";
    `P
      "This will automatically fetch rules for your project from the Semgrep \
       Registry. NOTE: Using `--config auto` will log in to the Semgrep \
       Registry with your project URL.";
    `P "For more information about Semgrep, go to https://semgrep.dev.";
    `P
      "NOTE: By default, Semgrep will report pseudonymous usage metrics to its \
       server if you pull your configuration from the Semgrep registy. To \
       learn more about how and why these metrics are collected, please see \
       https://semgrep.dev/docs/metrics. To modify this behavior, see the \
       --metrics option below.";
  ]
  @ CLI_common.help_page_bottom

let main argv =
  let info = Cmd.info "semgrep scan" ~doc ~man in
  CLI_common.safe_run run |> cmdline_term |> Cmd.v info |> Cmd.eval' ~argv
