(* The result of parsing a 'semgrep whoami' command *)
type conf = { name_only : bool }

(* entry point *)
val parse_argv : string array -> conf
