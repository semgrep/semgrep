
(* Print the set of tokens in a c++ file *)
val test_tokens_cpp :
  Common.filename -> unit
val test_parse_cpp:
  ?lang:Flag_parsing_cpp.language -> Common.filename list -> unit
val test_dump_cpp:
  Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_cpp foo.cpp will call the
 * test_parse_cpp function.
*)
val actions : unit -> Common.cmdline_actions
