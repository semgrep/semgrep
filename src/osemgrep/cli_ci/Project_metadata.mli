(** Collect information about the project from the environment.
    This module is a translation of [meta.py] renamed to [Project_metadata.ml]. *)

type 'e t = {
  repository : string;  (** The name of the repository. *)
  repo_url : Uri.t option;  (** The URL of the repository. *)
  branch : string option;  (** The branch name. *)
  ci_job_url : Uri.t option;  (** The URL of the CI. *)
  commit : Digestif.SHA1.t option;  (** The commit. *)
  commit_author_email : Emile.mailbox option;
      (** The email of the committer. *)
  commit_author_name : string option;  (** The name of the committer. *)
  commit_author_username : string option;  (** The username of the committer. *)
  commit_author_image_url : Uri.t option;
      (** The URL of the committer's image. *)
  commit_title : string option;  (** The commit's title. *)
  on : string option;  (** The event name. *)
  pull_request_author_username : string option;
      (** The username of the PR author. *)
  pull_request_author_image_url : Uri.t option;
      (** The URL of the PR author's image. *)
  pull_request_id : string option;  (** The Pull-Request ID. *)
  pull_request_title : string option;  (** The Pull-Request title. *)
  scan_environment : string option;  (** Kind of environement. *)
  is_full_scan : bool;
      (** Check if the current Git repository has enough to determine the
          [merge_base_ref] value. We assume:

          {[
            match Cmdliner.Term.eval (S.env, fake_info) with
            | `Ok env ->
              let t = S.make env in
              S.get_merge_base_ref env >>= fun merge_base_ref ->
              assert(t.is_full_scan = Option.is_some merge_base_ref)
          ]} *)
  extension : 'e;  (** Some environments extend metadata. *)
}

module type S = sig
  type env
  type extension

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
  val make : env -> extension t
  val term : extension t Cmdliner.Term.t
end

val get_url_from_sstp_url : string option -> Uri.t option
val get_repo_name_from_repo_url : string option -> string option
val git_check_output : Bos.Cmd.t -> string
