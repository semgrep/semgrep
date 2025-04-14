open Common.Operators

(* e.g. yield
 * TODO: may yield false TSAN positives on multicore *)
let facebook_lang_extensions = ref true

(* coupling: copy paste of Php_vs_php *)
let is_metavar_name s = s =~ "[A-Z]\\([0-9]?_[A-Z]*\\)?"

let cmdline_flags_pp () =
  [ ("-no_fb_ext", Arg.Clear facebook_lang_extensions, " ") ]
