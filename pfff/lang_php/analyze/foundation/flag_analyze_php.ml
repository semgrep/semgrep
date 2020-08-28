
(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_database = ref true 
let show_errors = ref true 
let verbose_typing = ref true 
let verbose_checking = ref true
let verbose_entity_finder = ref false
let debug_bdb = ref false
let debug_checker = ref false

let cmdline_flags_verbose () = [
  "-verbose_database", Arg.Set verbose_database,
  "  ";
  "-debug_bdb", Arg.Set debug_bdb,
  "  ";
  "-no_verbose_checking", Arg.Clear verbose_checking,
  " ";
  "-noverbose_database", Arg.Clear verbose_database,
  " ";
  "-debug_checker", Arg.Set debug_checker ,
  "  ";
]

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let check_normalized = ref true

let cmdline_flags_checks () = [
  "-no_check_normalized", Arg.Clear check_normalized, 
  " ";
]
