exception Error of string

(* this does not raise Error (but bailout with SCA_version.Other) *)
val parse : string -> SCA_version.t

(* used in Parse_rule.ml, may raise Error *)
val parse_constraints : string -> SCA_pattern.version_constraints
