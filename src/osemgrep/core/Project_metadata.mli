(* Collect information about the project from the environment *)

(* What is sent to the Semgrep backend in the scan_request. This is now
 * defined in semgrep_output_v1.atd so it can be reused in the backend.
 *)
type t = Semgrep_output_v1_t.project_metadata

(* Each Xxx_metadata.ml can implement this signature *)
module type S = sig
  (* usually a record representing a set of environment variables *)
  type env

  (* Extract the environment variable via Cmdliner.
   * Why using cmdliner? Why not simply access them with Sys.getenv_opt?
   * The advantage of cmdliner is that we can document those variables
   * and the env constant below can be combined in Ci_CLI.ml so those
   * variables can be part of the 'semgrep ci' man page!!
   *)
  val env : env Cmdliner.Term.t

  (* entry point *)
  val make : env -> t
end

(* a few helpers *)
val get_url_from_sstp_url : string option -> Uri.t option
val get_repo_name_from_repo_url : string option -> string option
