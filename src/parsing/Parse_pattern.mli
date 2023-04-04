val parse_pattern : ?print_errors:bool -> Lang.t -> string -> Pattern.t
val parse_pattern_ref : (bool -> Lang.t -> string -> Pattern.t) ref
