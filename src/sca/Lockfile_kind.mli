type t = Semgrep_output_v1_t.lockfile_kind (* = Input_to_core.lockfile_kind *)
[@@deriving show, eq, yojson]

(* for the 'string wrap' in Input_to_core.atd *)
val unwrap : t -> string
val wrap : string -> t

(* used in Core_scan.ml *)
val to_ecosystem : t -> Semgrep_output_v1_t.ecosystem
