type t =
  | Text
  | Json
  | Emacs
  | Vim
  | Sarif
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  (* used to disable the final display of match results because
   * we displayed them incrementally instead
   *)
  | TextIncremental
[@@deriving show]
