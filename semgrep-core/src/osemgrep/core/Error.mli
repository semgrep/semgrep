exception Semgrep_error of string * Exit_code.t option

(* shortcut *)
val abort : string -> 'a
