type t = Semgrep_output_v1_t.lockfile_kind [@@deriving show, eq]

(* used in Core_scan.ml *)
val to_ecosystem : t -> Semgrep_output_v1_t.ecosystem
