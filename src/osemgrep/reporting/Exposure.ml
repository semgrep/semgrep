open Common
module J = JSON
module Out = Semgrep_output_v1_t

type t = Reachable | Undetermined | Unreachable

let string_of = function
  | Reachable -> "reachable"
  | Undetermined -> "undetermined"
  | Unreachable -> "unreachable"

(* python: from rule_match.py exposure_type() *)
let of_cli_match_opt (cli_match : Out.cli_match) : t option =
  let* { reachable; _ } = cli_match.extra.sca_info in
  let metadata = JSON.from_yojson cli_match.extra.metadata in
  match JSON.member "sca-kind" metadata with
  | Some (J.String "upgrade-only") -> Some Reachable
  | Some (J.String "legacy") -> Some Undetermined
  (* TODO: stricter: raise error if Some else_json *)
  | _ -> if reachable then Some Reachable else Some Unreachable
