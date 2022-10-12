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
val parse_argv : string array -> (conf, Exit_code.t) result
