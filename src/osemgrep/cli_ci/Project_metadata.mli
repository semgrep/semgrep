(* Collect information about the project from the environment *)

type t = Semgrep_output_v1_t.project_metadata

(* Each Xxx_metadata.ml can implement this signature *)
module type S = sig
  type env

  (** Accessors. *)

  val get_event_name : env -> string option
  val get_repo_name : env -> string
  val get_repo_url : env -> Uri.t option
  val get_commit_sha : env -> Digestif.SHA1.t option
  val get_ci_job_url : env -> Uri.t option
  val get_pr_id : env -> string option
  val get_pr_title : env -> string option
  val get_branch : env -> string option
  val get_merge_base_ref : env -> Digestif.SHA1.t option Lwt.t

  (** Cmdliner helpers. *)

  val env : env Cmdliner.Term.t
  val make : env -> t
  val term : t Cmdliner.Term.t
end

val get_url_from_sstp_url : string option -> Uri.t option
val get_repo_name_from_repo_url : string option -> string option
val git_check_output : Bos.Cmd.t -> string
