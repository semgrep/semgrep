open Common.Operators

let strict_lexer = ref false
let short_open_tag = ref true

(* PHP is case insensitive, which is I think a bad idea, so
 * by default let's have a case sensitive lexer.
 *)
let case_sensitive = ref true

(* e.g. yield *)
let facebook_lang_extensions = ref true

(* coupling: copy paste of Php_vs_php *)
let is_metavar_name s = s =~ "[A-Z]\\([0-9]?_[A-Z]*\\)?"

let cmdline_flags_pp () =
  [ ("-no_fb_ext", Arg.Clear facebook_lang_extensions, " ") ]
