val parse_pattern : Lang.t -> ?print_errors:bool -> string -> Pattern.t
val parse_pattern_ref : (Lang.t -> bool -> string -> Pattern.t) ref
