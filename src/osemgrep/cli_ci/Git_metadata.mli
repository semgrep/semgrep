(* Gather metadata from local filesystem (we expect, at least, a Git
    repository) of from semgrep-specific environment variables
*)

type env = {
  _SEMGREP_REPO_NAME : string option;
  _SEMGREP_REPO_DISPLAY_NAME : string option;
  _SEMGREP_REPO_URL : Uri.t option;
  _SEMGREP_COMMIT : Digestif.SHA1.t option;
  _SEMGREP_JOB_URL : Uri.t option;
  _SEMGREP_PR_ID : string option;
  _SEMGREP_PR_TITLE : string option;
  _SEMGREP_BRANCH : string option;
}

(* Extract the environment variable via Cmdliner.
 * Why using cmdliner? Why not simply access them with Sys.getenv_opt?
 * The advantage of cmdliner is that we can document those variables
 * and the env constant below can be combined in Ci_CLI.ml so those
 * variables can be part of the 'semgrep ci' man page!!
 *)
val env : env Cmdliner.Term.t

class meta :
  < Cap.exec > ->
  scan_environment:string ->
  baseline_ref:Digestif.SHA1.t option ->
  env ->
object
  method project_metadata : Semgrep_output_v1_t.project_metadata

  (* to be overriden in the children *)
  method branch : string option
  method ci_job_url : Uri.t option
  method commit_sha : Digestif.SHA1.t option
  method event_name : string
  method is_full_scan : bool
  method pr_id : string option
  method pr_title : string option
  method repo_name : string
  method repo_display_name : string
  method repo_url : Uri.t option
  method merge_base_ref : Digestif.SHA1.t option
end
