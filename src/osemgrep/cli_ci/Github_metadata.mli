type env = {
  git : Git_metadata.env;
  _GITHUB_EVENT_JSON : Yojson.Basic.t;
  _GITHUB_REPOSITORY : string option;
  _GITHUB_API_URL : Uri.t option;
  _GITHUB_SHA : Digestif.SHA1.t option;
  _GITHUB_SERVER_URL : Uri.t;
  _GITHUB_REF : string option;
  _GITHUB_HEAD_REF : string option;
  _GITHUB_RUN_ID : string option;
  _GITHUB_EVENT_NAME : string option;
  _GH_TOKEN : string option;
}

include Project_metadata.S with type env := env
(** Gather metadata from GitHub Actions. *)

class meta :
  baseline_ref:Digestif.SHA1.t option ->
  Git_metadata.env ->
  env ->
object
  method project_metadata : Semgrep_output_v1_t.project_metadata
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
