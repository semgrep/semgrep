open Common
module Out = Semgrep_output_v1_t

type sca_operator = Eq | Gte | Lte | Gt | Lt
[@@deriving show { with_path = false }, eq]

(* Something like (>= 2.0.0) or (== 5.1.7) *)
type version_constraint = { op : sca_operator; version : SCA_version.t }
[@@deriving show, eq]

(* A pattern to match against versions in a lockfile.
   This is not like a regular code pattern! It's description of a range of
   versions.
   For example: ">=1.0.0, <= 2.3.5", which is meant to "match" any version in
   that interval, e.g. 1.3.5
*)
type t = {
  ecosystem : Out.ecosystem;
  package_name : string;
  version_constraints : version_constraints;
}

(* An intersection of constraints, like (>= 1.0.0, < 3.0.0), meaning
 * "greater than or equal 1.0.0 and less than 3.0.0".
 * We don't have union/an actual tree of constraints because of the historical
 * baggage of the structure of supply chain rules, which only have top-level
 * union.
 *)
and version_constraints = SCA_And of version_constraint list
[@@deriving show { with_path = false }, eq]

(* Pretty printer
 * alt: store a version_constraints_string above
 *)
let version_constraints_to_string (SCA_And xs) =
  xs
  |> List_.map (fun { op; version } ->
         let op_str =
           match op with
           | Eq -> "==" (* or = ? *)
           | Gte -> ">="
           | Lte -> "<="
           | Gt -> ">"
           | Lt -> "<"
         in
         spf "%s %s" op_str (SCA_version.to_string version))
  |> String.concat ", "
