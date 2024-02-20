module Out = Semgrep_output_v1_j

(* The "core of a version": a dot separated list of numbers, like 4.1.6.2.7 *)
type version_core = { major : int; minor : int; incrementals : int list }
[@@deriving show, eq]

(* Versions are sometimes listed as arbitrary strings, like a github URL *)
type version = Version of version_core | Other of string [@@deriving show, eq]
type constraint_ = Eq | Gte | Lte | Gt | Lt [@@deriving show, eq]

(* Something like (>= 2.0.0) or (== 5.1.7) *)
type version_constraint = { version : version; constraint_ : constraint_ }
[@@deriving show, eq]

(* An intersection of constraints, like (>= 1.0.0, < 3.0.0), meaning "greater than or equal 1.0.0 and less than 3.0.0" *)
(* We don't have union/an actual tree of constraints because of the historical baggage of the structure of supply chain rules,
   which only have top-level union.
*)
type constraint_ast = And of version_constraint list [@@deriving show, eq]

type t = {
  package_name : string;
  package_version : version;
  package_version_string : string;
  ecosystem : Out.ecosystem;
  transitivity : Out.transitivity;
  url : Uri.t option;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}
[@@deriving show, eq]

(* A dependency in a manifest may have a version range like >=1.0.0, and they are *direct* by definition
   Contains only an unparsed string for it's package_version_constraint because we never actually use it
   for anything, so parsing it is pointless
*)
type manifest_dependency = {
  package_name : string;
  package_version_constraint_string : string;
  ecosystem : Out.ecosystem;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}
[@@deriving show, eq]
