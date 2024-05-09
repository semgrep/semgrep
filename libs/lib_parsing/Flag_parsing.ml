(* TODO: we should probably get rid of this file.
 * You can switch to Logs src if you want logging for your parser.
 *)

let verbose_lexing = ref false
let verbose_parsing = ref true

(* see Parse_info.lexical_error helper and Lexical_error exn *)
let exn_when_lexical_error = ref true

(* Do not raise an exn when a parse error but use NotParsedCorrectly.
 * If the parser is quite complete, it's better to set
 * error_recovery to false by default and raise a true ParseError exn.
 * This can be used also in testing code, to parse a big set of files and
 * get statistics (e.g., -parse_java) and not stop at the first parse error.
 *)
let error_recovery = ref false
let debug_lexer = ref false
let debug_parser = ref false

(* TODO: definitely switch to Logs src for that *)
let show_parsing_error = ref true

(* will lexer $X and '...' tokens, and allow certain grammar extension
 * see sgrep_guard() below.
 *)
let sgrep_mode = ref false

let cmdline_flags_verbose () =
  [
    ("-no_verbose_parsing", Arg.Clear verbose_parsing, "  ");
    ("-no_verbose_lexing", Arg.Clear verbose_lexing, "  ");
  ]

let cmdline_flags_debugging () =
  [
    ("-debug_lexer", Arg.Set debug_lexer, " ");
    ("-debug_parser", Arg.Set debug_parser, " ");
  ]

let sgrep_guard v = if !sgrep_mode then v else raise Parsing.Parse_error
