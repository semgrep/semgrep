(*
   Print the matched lines to stdout in a human-readable format.
*)
val print :
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  Src_file.t ->
  Match.match_ list ->
  unit

(*
   Print the results of matching multiple patterns against multiple documents.
*)
val print_nested_results :
  ?with_time:bool ->
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  (Src_file.t
  * (Match.pattern_id * Match.match_ list * float) list
  * float
  * float)
  list ->
  (Src_file.t * Parse_pattern.error) list ->
  unit
