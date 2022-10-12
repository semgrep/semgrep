(*
   'semgrep scan' command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  autofix : bool;
  baseline_commit : string option;
  config : string;
  debug : bool;
  exclude : string list;
  include_ : string list;
  lang : string option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  pattern : string option;
  quiet : bool;
  respect_git_ignore : bool;
  target_roots : string list;
  timeout : float;
  timeout_threshold : int;
  verbose : bool;
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
