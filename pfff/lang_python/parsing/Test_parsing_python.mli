(*s: pfff/lang_python/parsing/Test_parsing_python.mli *)


(*s: signature [[Test_parsing_python.test_tokens_python]] *)
(* Print the set of tokens in a .py file *)
val test_tokens_python : Common.filename -> unit
(*e: signature [[Test_parsing_python.test_tokens_python]] *)

(*s: signature [[Test_parsing_python.actions]] *)
(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff -parse_python foo.py will call the 
 * test_parse_python function.
 *)
val actions : unit -> Common.cmdline_actions
(*e: signature [[Test_parsing_python.actions]] *)
(*e: pfff/lang_python/parsing/Test_parsing_python.mli *)
