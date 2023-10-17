(* See also Core_error.ml and semgrep_output_v1.atd error_type *)

exception Semgrep_error of string * Exit_code.t option
exception Exit of Exit_code.t

(* shortcut *)
val abort : string -> 'a
val exit : Exit_code.t -> 'a

(* used for CLI text output and for the metrics payload.errors.errors *)
val string_of_error_type : Semgrep_output_v1_t.error_type -> string
