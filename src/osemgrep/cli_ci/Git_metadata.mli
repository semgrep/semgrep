(** Gather metadata only from local filesystem. We expect, at least, a Git
    repository. *)

type env = {
  _SEMGREP_REPO_NAME : string option;
  _SEMGREP_REPO_URL : Uri.t option;
  _SEMGREP_COMMIT : Digestif.SHA1.t option;
  _SEMGREP_JOB_URL : Uri.t option;
  _SEMGREP_PR_ID : string option;
  _SEMGREP_PR_TITLE : string option;
  _SEMGREP_BRANCH : string option;
}

include Project_metadata.S with type env := env

class git_meta :
  ?scan_environment:string ->
  baseline_ref:Digestif.SHA1.t option ->
  env ->
object
  method project_metadata : Semgrep_output_v1_t.project_metadata

  (* to be overriden in the children *)
  method branch : string option
  method ci_job_url : Uri.t option
  method commit_sha : Digestif.SHA1.t option
  method commit_timestamp : string option
  method event_name : string
  method is_full_scan : bool
  method pr_id : string option
  method pr_title : string option
  method repo_name : string
  method repo_url : Uri.t option
  method merge_base_ref : Digestif.SHA1.t option
end

(* used in Github_metadata.ml *)
val get_event_name : env -> string option
val get_branch : env -> string option
val get_ci_job_url : env -> Uri.t option
val get_repo_url : env -> Uri.t option
