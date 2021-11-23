(* may raise all the exns in Rule *)
val parse : Common.filename -> Rule.rules

(* this can be used for parsing -e/-f extended patterns in Run_semgrep.ml *)
val parse_xpattern : Xlang.t -> string Rule.wrap -> Rule.xpattern
