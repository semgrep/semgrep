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

(* used in Github_metadata.ml *)
val get_event_name : env -> string option
val get_branch : env -> string option
val get_ci_job_url : env -> Uri.t option
val get_repo_url : env -> Uri.t option
