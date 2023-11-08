(* TODO merge with src/core/Engine_kind.ml *)

type analysis_flavor = OSS_intrafile | Deep_intrafile | Deep_interfile
[@@deriving show]

type secrets_config = { allow_all_origins : bool } [@@deriving show]

type pro_flavor = {
  (* Right now code_config is inlined because it really only has one nob = extra_languages, after talking
     with Raghav and milan analysis_flavor is really a shared feature of all products using
     the pro-engine. *)
  extra_languages : bool;
  analysis : analysis_flavor;
  secrets_config : secrets_config option; (* None = Disabled *)
}
[@@deriving show]

type t = OSS | PRO of pro_flavor [@@deriving show]

let make ~extra_languages ~analysis ~secrets_config =
  match (extra_languages, analysis, secrets_config) with
  | false, OSS_intrafile, None -> OSS
  | _ -> PRO { extra_languages; analysis; secrets_config }
