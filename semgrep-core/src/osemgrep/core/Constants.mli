type output_format =
  | Text
  | Json
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif
  | Emacs
  | Vim

type rule_severity = Info | Warning | Error | Inventory | Experiment

val default_max_target_size : int
val default_timeout : int
