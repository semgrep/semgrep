type version_core = { major : int; minor : int; incrementals : int list }
and version = Version of version_core | Other of string
and constraint_ = Eq | Gte | Lte | Gt | Lt
and version_constraint = { version : version; constraint_ : constraint_ }

and t = {
  package_name : string;
  package_version : version;
  package_version_string : string;
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
  package_version_constraint : version_constraint;
  ecosystem : ecosystem;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}
[@@deriving show, eq]
