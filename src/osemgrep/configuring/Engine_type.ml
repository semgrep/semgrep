(* TODO merge with src/core/Engine_kind.ml *)
type pro_flavor = Language_only | Intrafile | Interfile [@@deriving show]
type t = OSS | PRO of pro_flavor [@@deriving show]
