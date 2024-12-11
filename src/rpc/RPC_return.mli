module Out = Semgrep_output_v1_j

val autofix : bool -> Out.edit list -> int * (int * string list) list
val format : Out.output_format -> Out.format_context -> Out.cli_output -> string

val sarif_format :
  < tmp : Cap.FS.tmp > ->
  Out.fpath (* path to a temporary files containing the rules *) ->
  Out.format_context ->
  bool ->
  string ->
  bool ->
  Out.cli_match list ->
  Out.cli_error list ->
  string * float

val contributions : < Cap.exec > -> Out.contributions
val validate : Out.fpath -> bool

val hook_resolve_dependencies :
  (< Cap.exec ; Cap.tmp > ->
  Out.dependency_source list ->
  (Out.dependency_source * Out.resolution_result) list)
  option
  ref

val hook_dump_rule_partitions :
  (Out.raw_json -> int -> Fpath.t -> bool) option ref
