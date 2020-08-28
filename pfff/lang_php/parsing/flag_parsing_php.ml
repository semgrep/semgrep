(*s: flag_parsing_php.ml *)
(*x: flag_parsing_php.ml *)
(*x: flag_parsing_php.ml *)
(*x: flag_parsing_php.ml *)
(*x: flag_parsing_php.ml *)

let strict_lexer = ref false

(*x: flag_parsing_php.ml *)
let short_open_tag = ref true
(*x: flag_parsing_php.ml *)

(* PHP is case insensitive, which is I think a bad idea, so
 * by default let's have a case sensitive lexer.
 *)
let case_sensitive = ref true

(* e.g. yield *)
let facebook_lang_extensions = ref true

(* in facebook context, we want xhp support by default *)
let xhp_builtin = ref true

let verbose_pp = ref false
(* Alternative way to get xhp by calling xhpize as a preprocessor.
 * Slower than builtin_xhp and have some issues where the comments
 * are removed, unless you use the experimental_merge_tokens_xhp
 * but which has some issues itself. 
 *)
let pp_default = ref (None: string option)
let xhp_command = "xhpize" 
let obsolete_merge_tokens_xhp = ref false

(*s: flag_parsing_php.ml pp related flags *)

open Common

(* coupling: copy paste of Php_vs_php *)
let is_metavar_name s = 
  s =~ "[A-Z]\\([0-9]?_[A-Z]*\\)?"

let cmdline_flags_pp () = [
  "-pp", Arg.String (fun s -> pp_default := Some s),
  " <cmd> optional preprocessor (e.g. xhpize)";
  "-verbose_pp", Arg.Set verbose_pp, 
  " ";
  (*s: other cmdline_flags_pp *)
  "-xhp", Arg.Set xhp_builtin,
  " parsing XHP constructs (default)";
  "-xhp_with_xhpize", Arg.Unit (fun () -> 
    xhp_builtin := false;
    pp_default := Some xhp_command),
  "  parsing XHP using xhpize as a preprocessor";
  "-no_xhp", Arg.Clear xhp_builtin,
  " ";
  "-no_fb_ext", Arg.Clear facebook_lang_extensions,
  " ";
  (*e: other cmdline_flags_pp *)
]
(*e: flag_parsing_php.ml pp related flags *)
(*e: flag_parsing_php.ml *)
