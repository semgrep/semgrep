type t = Semgrep_output_v1_j.manifest_kind (* = Input_to_core.manifest_kind *)
[@@deriving show, eq, yojson]

(* for the 'string wrap' in Input_to_core.atd *)
val unwrap : t -> string
val wrap : string -> t
val to_ecosystem : t -> Semgrep_output_v1_j.ecosystem
