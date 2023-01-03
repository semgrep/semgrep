val add_fake_node_when_undefined_entity : bool ref

val build :
  ?verbose:bool ->
  ?logfile:Common.filename ->
  ?readable_file_format:bool ->
  ?only_defs:bool ->
  ?is_skip_error_file:(Common.filename -> bool) ->
  ?class_analysis:bool ->
  Common.dirname ->
  Common.filename list ->
  Graph_code.t * Graph_code.statistics

(* used by scheck *)
type resolved_name = R of string

val lookup_inheritance :
  Graph_code.t ->
  resolved_name * string ->
  'a ->
  ((resolved_name * 'a) * Entity_code.entity_kind) option
