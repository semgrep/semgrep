module In = Input_to_core_t
module OutJ = Semgrep_output_v1_t

(* Note that both of these together imply that In.product equals Out.product *)
let _proof_that_input_subtype_output_product (x : In.product) : OutJ.product = x
let _proof_that_output_subtype_input_product (x : OutJ.product) : In.product = x
let all = [ `SAST; `SCA; `Secrets ]

let of_cli_match (m : OutJ.cli_match) =
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
