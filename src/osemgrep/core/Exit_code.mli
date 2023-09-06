(*
   Exit codes of the semgrep executable (not just 'semgrep scan').

   Some of those exit codes are also (ab)used to represent some error code
   for errors reported in the semgrep CLI JSON output.
*)

(* This ensures that exit codes are declared and documented here. *)
type t = private int

(* 'to_int x' is the same as '(x :> int)'. *)
val to_int : t -> int
val of_int : int -> t

(* Short error message describing the meaning of each exit code *)
val to_message : t -> string

(*
   Standard exit codes.
   All calls to exit must use one of these.
*)
val ok : t
val findings : t
val fatal : t
val invalid_code : t
val invalid_pattern : t
val unparseable_yaml : t
val missing_config : t
val invalid_language : t
val invalid_api_key : t
val scan_fail : t

(* to remove at some point *)
val not_implemented_in_osemgrep : t
