(* This modules uses the ocaml type system to prove that the
   Input_to_core_t.product is the same as
   Semgrep_output_v1_t.product. *)

val all : Semgrep_output_v1_t.product list
val of_cli_match : Semgrep_output_v1_t.cli_match -> Semgrep_output_v1_t.product
