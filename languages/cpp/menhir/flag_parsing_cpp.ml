(*****************************************************************************)
(* types *)
(*****************************************************************************)

type language = C | Cplusplus

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macros_h =
  ref
    (Fpath.v
       (match Sys.win32 with
       (* Windows doesn't like paths that start with / *)
       (* So provide a default path that may exist in windows *)
       | true -> "C:/Program Files/LLVM/include/c++/v1/macros.h"
       | false -> "/data/cpp_stdlib/macros.h"))

let cmdline_flags_macrofile () =
  [
    ( "-macros",
      Arg.String (fun s -> macros_h := Fpath.v s),
      " <file> list of macros to expand" );
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

(* TODO: switch to Log_parser_cpp, or design new Log srcs for each of those
 * category
 *)
let debug_typedef = ref true
let debug_pp = ref true
let debug_cplusplus = ref true
let debug_pp_ast = ref false

let cmdline_flags_debugging () =
  [
    ("-no_debug_pp", Arg.Clear debug_pp, " ");
    ("-no_debug_typedef", Arg.Clear debug_typedef, "  ");
    ("-no_debug_cplusplus", Arg.Clear debug_cplusplus, " ");
    ( "-debug_cpp",
      Arg.Unit
        (fun () ->
          debug_pp := true;
          debug_typedef := true;
          debug_cplusplus := true),
      " " );
  ]

(*****************************************************************************)
(* Disable parsing features *)
(*****************************************************************************)

let strict_lexer = ref false
let if0_passing = ref true
let ifdef_to_if = ref false
