(*
   Exception and error management for semgrep-core
*)

(*****************************************************************************)
(* Main error type *)
(*****************************************************************************)

type t = {
  rule_id : Rule_ID.t option;
  typ : Semgrep_output_v1_t.error_type;
  loc : Tok.location;
  msg : string;
  (* ?? diff with msg? *)
  details : string option;
}
[@@deriving show]

module ErrorSet : Set.S with type elt = t

(*****************************************************************************)
(* Converter functions *)
(*****************************************************************************)

val mk_error :
  Rule_ID.t option ->
  Tok.location ->
  string ->
  Semgrep_output_v1_t.error_type ->
  t

(* Convert an invalid rule into an error.
   TODO: return None for rules that are being skipped due to version
   mismatches.
*)
val error_of_invalid_rule_error : Rule.invalid_rule_error -> t
val error_of_rule_error : file:string -> Rule.error -> t

(* Convert a caught exception and its stack trace to a Semgrep error.
 * See also JSON_report.json_of_exn for non-target related exn handling.
 *)
val exn_to_error : Rule_ID.t option -> string (* filename *) -> Exception.t -> t

(*****************************************************************************)
(* Try with error *)
(*****************************************************************************)

val try_with_result_to_error :
  Fpath.t -> (unit -> ('a, t) Result.t) -> ('a, t) Result.t

val try_with_log_exn_and_reraise : Fpath.t -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : t -> string

val severity_of_error :
  Semgrep_output_v1_t.error_type -> Semgrep_output_v1_t.error_severity
