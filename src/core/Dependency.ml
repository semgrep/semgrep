(* The "core of a version": a dot separated list of numbers, like 4.1.6.2.7 *)
type version_core = { major : int; minor : int; incrementals : int list }

(* Versions are sometimes listed as arbitrary strings, like a github URL *)
and version = Version of version_core | Other of string
and constraint_ = Eq | Gte | Lte | Gt | Lt

(* Something like (>= 2.0.0) or (== 5.1.7) *)
and version_constraint = { version : version; constraint_ : constraint_ }

(* An intersection of constraints, like (>= 1.0.0, < 3.0.0), meaning "greater than or equal 1.0.0 and less than 3.0.0" *)
(* We don't have union/an actual tree of constraints because of the historical baggage of the structure of supply chain rules,
   which only have top-level union.
*)
and constraint_ast = And of version_constraint list

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
  package_version_constraint_string : string;
  ecosystem : ecosystem;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}
[@@deriving show, eq]
