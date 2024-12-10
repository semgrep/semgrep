exception Error of string

(* may raise Error *)
val parse : string -> SCA_version.t

(* used in Parse_rule.ml, may also raise Error *)
val parse_constraints : string -> SCA_pattern.version_constraints
