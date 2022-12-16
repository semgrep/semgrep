
(* e.g. "10.1" *)
type section = string

type code_excerpt = string list

type comment_style =
  string (* comment_start *) * string (* comment_end *)

type skeleton =
  (string (* section1 *) *
   ((string (* section2 title *) * section) list))
    list

type sections = (section, code_excerpt) Common.assoc

val parse_data_file:
  Common.filename -> sections

val parse_skeleton_file:
  Common.filename -> skeleton

val detect_comment_style:
  Common.filename -> comment_style

type gen_mode =
  | OneFilePerSection
  | OneDirPerSection

val gen_source_files:
  skeleton -> sections -> comment_style ->
  gen_mode:gen_mode ->
  output_dir:Common.dirname ->
  ext_file:string ->
  hook_start_section2:(string -> string) ->
  hook_line_body:(string -> string) ->
  hook_end_section2:(string -> string) ->
  unit
