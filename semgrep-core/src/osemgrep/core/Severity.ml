(* was in constants.py, but not really constants, more like types! *)

(* coupling: ensure consistency with 'serverity' in 'rule_schema_v1.yaml'
 * LATER: redundant with Rule.severity in semgrep-core.
 *)
type rule_severity = Info | Warning | Error | Inventory | Experiment
(* TOPORT
    @classmethod
    def _missing_(cls: Type[Enum], value: object) -> Enum:
        if not isinstance(value, str):
            raise TypeError(f"invalid rule severity type: {type(value)}")
        for member in cls:
            if member.value.lower() == value:
                return member
        raise ValueError(f"invalid rule severity value: {value}")
*)
