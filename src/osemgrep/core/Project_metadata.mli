(* Collect information about the project from the environment *)

(* What is sent to the Semgrep backend in the scan_request. This is now
 * defined in semgrep_output_v1.atd so it can be reused in the backend.
 *)
type t = Semgrep_output_v1_t.project_metadata

(* a few helpers *)
val get_url_from_sstp_url : string option -> Uri.t option
val get_repo_name_from_repo_url : string option -> string option
