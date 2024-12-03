type t = Semgrep_output_v1_t.product [@@deriving show]

(* currently [`SAST; `SCA; `Secrets] *)
val all : t list

(* use the metadata.product field to derive the product of a match *)
val of_cli_match : Semgrep_output_v1_t.cli_match -> t
