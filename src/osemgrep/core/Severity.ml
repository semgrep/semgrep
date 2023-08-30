(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rules, findings, and errors severity.
 *
 * TODO: there are (too) many places where we define severity:
 *  - rule_schema_v1.yaml
 *  - Rule.severity in semgrep-core
 *  - core_severity in semgrep_output_v1.atd
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

(* for CLI --severity input *)
let converter =
  Cmdliner.Arg.enum
    [ ("INFO", `Info); ("WARNING", `Warning); ("ERROR", `Error) ]

(* for CLI --severity filtering *)
let of_rule_severity_opt (x : Rule.severity) : t option =
  match x with
  | Rule.Error -> Some `Error
  | Rule.Warning -> Some `Warning
  | Rule.Info -> Some `Info
  | Rule.Inventory
  | Rule.Experiment ->
      None
