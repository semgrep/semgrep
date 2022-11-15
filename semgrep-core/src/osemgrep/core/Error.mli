exception Semgrep_error of string * Exit_code.t option
exception Exit of Exit_code.t

(* shortcut *)
val abort : string -> 'a
val exit : Exit_code.t -> 'a
