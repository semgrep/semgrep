type t =
  | Text
  | Json
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif
  | Emacs
  | Vim
  (* used to disable the final display of match results because
   * we displayed them incrementally instead
   *)
  | TextIncremental
[@@deriving show]
