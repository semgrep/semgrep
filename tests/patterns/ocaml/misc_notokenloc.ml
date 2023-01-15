(* if you want to artificially create some NoTokenLocation error to
 * test some fault-tolerance code, modify parser_ml.mly 
 * constr_longident rule as is:
 | "(" ")"                             { [], ("()", Parse_info.fake_info "XX") }
 *
 * and then write a pattern like $X which will match everything.
 *)
(* ERROR: match *)
()

