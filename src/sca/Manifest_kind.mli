type t = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq]

val to_ecosystem : t -> Semgrep_output_v1_j.ecosystem
