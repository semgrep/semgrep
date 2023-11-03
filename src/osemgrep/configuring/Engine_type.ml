(* TODO merge with src/core/Engine_kind.ml *)
(* TODO Intrafile -> Intraprocedural and remove deep *)
type analysis_flavor = Intrafile | Deep_intrafile | Deep_interfile
[@@deriving show]

type secrets_config = { allow_all_origins : bool } [@@deriving show]

type pro_flavor = {
  extra_languages : bool;
  analysis : analysis_flavor;
  secrets_config : secrets_config option; (* None = Disabled *)
}
[@@deriving show]

type t = OSS | PRO of pro_flavor [@@deriving show]

let make ~extra_languages ~analysis ~secrets_config =
  match (extra_languages, analysis, secrets_config) with
  | false, Intrafile, None -> OSS
  | _ -> PRO { extra_languages; analysis; secrets_config }
