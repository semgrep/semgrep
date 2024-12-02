module In = Input_to_core_t
module Out = Semgrep_output_v1_t

type t = Semgrep_output_v1_t.product (* = Input_to_core.product *)
[@@deriving show]

(* This modules uses the ocaml type system to prove that the
   Input_to_core_t.product is the same as
   Semgrep_output_v1_t.product.
   alt: have a proper module system in ATD.
*)

(* Note that both of these together imply that In.product equals Out.product *)
let _proof_that_input_subtype_output_product (x : In.product) : Out.product = x
let _proof_that_output_subtype_input_product (x : Out.product) : In.product = x
let all : t list = [ `SAST; `SCA; `Secrets ]

let of_cli_match (m : Out.cli_match) : t =
  (* alt: define a proper field in cli_match instead of abusing metadata *)
  let metadata_product_opt =
    try
      match Yojson.Basic.Util.member "product" m.extra.metadata with
      | `String "secrets" -> Some `Secrets
      | `String "sca" -> Some `SCA
      | `String ("code" | "sast") -> Some `SAST
      | _ -> None
    with
    | _ -> None
  in
  match metadata_product_opt with
  (* Not sure if this is correct. Assuming the product is SCA if there
     is sca_info. *)
  | None when Option.is_some m.extra.sca_info -> `SCA
  | Some p -> p
  | _ -> `SAST
