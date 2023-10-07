(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rules, findings, and errors severity.
 *
 * TODO: there are (too) many places where we define severity:
 *  - rule_schema_v1.yaml
 *  - error_severity and rule_severity in semgrep_output_v1.atd
 *  - DONE level in Error.ml
 * We should remove some of those.
 *
 * python: was in constants.py, but not really constants, more like types!
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* valid for rules and for findings, but also for semgrep errors
   found in rules, and in semgrep --severity
   TODO: remove, use semgrep_output_v1.error_severity instead
*)
type t = [ `Error | `Warning | `Info ] [@@deriving show]

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* for CLI JSON output *)
let to_string (x : t) =
  match x with
  | `Warning -> "warn"
  | `Error -> "error"
  | `Info -> "info"

(* TOPORT?
    def _missing_(cls: Type[Enum], value: object) -> Enum:
        if not isinstance(value, str):
            raise TypeError(f"invalid rule severity type: {type(value)}")
        for member in cls:
            if member.value.lower() == value:
                return member
        raise ValueError(f"invalid rule severity value: {value}")
*)

(* TODO: for this we should actually allow rule_severity, not just
 * error_severity
 *)
(* for CLI --severity input *)
let converter =
  Cmdliner.Arg.enum
    [ ("INFO", `Info); ("WARNING", `Warning); ("ERROR", `Error) ]

(* for CLI --severity filtering *)
let of_rule_severity_opt (x : Rule.severity) : t option =
  match x with
  | `Error -> Some `Error
  | `Warning -> Some `Warning
  | `Info -> Some `Info
  | `Inventory
  | `Experiment ->
      None
