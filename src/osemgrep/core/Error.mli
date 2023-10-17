exception Semgrep_error of string * Exit_code.t option
exception Exit of Exit_code.t

(* shortcut *)
val abort : string -> 'a
val exit : Exit_code.t -> 'a

(* used in text output and for the metrics payload.error.error *)
val string_of_error_type : Semgrep_output_v1_t.error_type -> string
