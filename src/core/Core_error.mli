(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Exception and error management for semgrep-core
*)

(*****************************************************************************)
(* Main error type *)
(*****************************************************************************)

type t = {
  typ : Semgrep_output_v1_t.error_type;
  msg : string;
  loc : Tok.location option;
  rule_id : Rule_ID.t option;
  details : string option;
}
[@@deriving show]

(* Useful if we want to raise a core_error *)
exception Unhandled_core_error of t

module ErrorSet : Set.S with type elt = t

(*****************************************************************************)
(* Converter functions *)
(*****************************************************************************)

val mk_error :
  ?rule_id:Rule_ID.t ->
  ?msg:string ->
  ?loc:Tok.location ->
  Semgrep_output_v1_t.error_type ->
  t

(* Convert an invalid rule into an error.
   TODO: return None for rules that are being skipped due to version
   mismatches.
*)
val error_of_invalid_rule : Rule_error.invalid_rule -> t
val error_of_rule_error : Rule_error.t -> t

(* Convert a caught exception and its stack trace to a Semgrep error.
 * See also JSON_report.json_of_exn for non-target related exn handling.
 *)
val exn_to_error : ?file:Fpath.t -> Exception.t -> t

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

val string_of_error : t -> string

val severity_of_error :
  Semgrep_output_v1_t.error_type -> Semgrep_output_v1_t.error_severity

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

val split_fixpoint_timeouts :
  t list -> t list * [> `Fixpoint_timeouts of t list ]
