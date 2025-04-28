open Common
module Out = Semgrep_output_v1_t

type kind = Out.sca_match_kind

let pp_kind fmt _kd = Format.fprintf fmt "SCA_match.kind: TODO"
let equal_kind kd1 kd2 = kd1 =*= kd2

type t = {
  (* the actual dependency in the lockfile *)
  dep : Dependency.t;
  (* the version constraint on a package and its ecosystem *)
  pat : SCA_pattern.t;
  kind : kind;
}
[@@deriving show, eq]
