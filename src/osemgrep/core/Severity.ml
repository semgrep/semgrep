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

(* valid for rules and for findings, but also for semgrep errors *)
type basic_severity = [ `Error | `Warning | `Alert ] [@@deriving show]

(* found in rules, and in semgrep --severity *)
type rule_severity = [ basic_severity | `Info ] [@@deriving show]

(* do we still need this? *)
type extended_severity = [ rule_severity | `Inventory | `Experiment ]
[@@deriving show]

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* for CLI JSON output *)
let string_of_basic_severity = function
  | `Warning -> "warn"
  | `Error -> "error"
  | `Alert -> "alert"

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
let rule_severity_of_rule_severity_opt (x : Rule.severity) :
    rule_severity option =
  match x with
  | Rule.Error -> Some `Error
  | Rule.Warning -> Some `Warning
  | Rule.Info -> Some `Info
  | Rule.Inventory
  | Rule.Experiment ->
      None
