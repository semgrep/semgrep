module Out = Semgrep_output_v1_t

type t = Semgrep_output_v1_t.product [@@deriving show]

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
