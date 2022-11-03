(*
   'semgrep scan' (and also 'semgrep ci') command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  autofix : bool;
  dryrun : bool;
  baseline_commit : string option;
  config : string;
  exclude : string list;
  include_ : string list;
  lang : string option;
  logging_level : Logs.level option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  output_format : Constants.output_format;
  pattern : string option;
  respect_git_ignore : bool;
  strict : bool;
  target_roots : string list;
  time_flag : bool;
  timeout : float;
  timeout_threshold : int;
}

(* Command-line defaults. *)
val default : conf

(*
   Usage: parse_argv [| "semgrep-scan"; <args> |]

   This function returns an exit code to be passed to the 'exit' function
   if there was an error parsing argv (Exit_code.fatal) or when
   using semgrep scan --help (Exit_code.ok), and the conf otherwise if everything
   went fine.
*)
val parse_argv : string array -> (conf, Exit_code.t) result

(* used by Ci_CLI.ml *)
val cmdline_term : conf Cmdliner.Term.t
