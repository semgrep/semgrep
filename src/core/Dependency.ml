type t = {
  package_name : string;
  package_version : string;
  ecosystem : ecosystem;
  transitivity : transitivity;
  url : string option;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}

(* TODO: the rest of them *)
and ecosystem = Npm
and transitivity = Direct | Transitive | Unknown [@@deriving show, eq]

(* A dependency in a manifest may have a version range like >=1.0.0, and they are *direct* by definition *)
type manifest_dependency = {
  package_name : string;
  package_version_range : string;
  ecosystem : ecosystem;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}
[@@deriving show, eq]
