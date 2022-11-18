type conf = {
  exclude : string list;
  include_ : string list;
  max_target_bytes : int;
  respect_git_ignore : bool;
  baseline_commit : string option;
  scan_unknown_extensions : bool;
}
[@@deriving show]

val get_targets :
  conf ->
  string list (* target roots *) ->
  Common.filename list * Output_from_core_t.skipped_target list
