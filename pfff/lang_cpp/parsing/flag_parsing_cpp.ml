
(*****************************************************************************)
(* types *)
(*****************************************************************************)

type language = 
  | C
  | Cplusplus

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macros_h = 
  ref (Filename.concat Config_pfff.path "/data/cpp_stdlib/macros.h")

let cmdline_flags_macrofile () = [
  "-macros", Arg.Set_string macros_h,
  " <file> list of macros to expand";
]

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_pp_ast = ref false

let filter_msg = ref false
let filter_classic_passed = ref false
let filter_define_error = ref true

(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_typedef = ref false
let debug_pp = ref false
let debug_pp_ast  = ref false
let debug_cplusplus = ref false

let cmdline_flags_debugging () = [
  "-debug_pp",          Arg.Set  debug_pp, " ";
  "-debug_typedef",     Arg.Set  debug_typedef, "  ";
  "-debug_cplusplus",   Arg.Set  debug_cplusplus, " ";

  "-debug_cpp", Arg.Unit (fun () ->
    debug_pp := true;
    debug_typedef := true;
    debug_cplusplus := true;
  ), " ";
]

(*****************************************************************************)
(* Disable parsing features *)
(*****************************************************************************)

let strict_lexer = ref false

let if0_passing = ref true
let ifdef_to_if  = ref false

