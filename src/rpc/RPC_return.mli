module Out = Semgrep_output_v1_j

val autofix : bool -> Out.edit list -> int * (int * string list) list

val sarif_format :
  < tmp : Cap.FS.tmp > ->
  bool ->
  string ->
  bool ->
  Out.fpath ->
  Out.cli_match list ->
  Out.cli_error list ->
  string * float

val contributions : < Cap.exec > -> Out.contributions
