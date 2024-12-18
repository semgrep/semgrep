type t = {
  package_name : string;
  package_version : SCA_version.t;
  package_version_string : string;
  ecosystem : SCA_ecosystem.t;
  transitivity : SCA_transitivity.t;
  url : Uri.t option;
  (* ?? locs in lockfile? *)
  loc : Tok.location * Tok.location;
  (* All the tokens from section of the generic AST we read the dependency from
     So all the tokens inside the range defined by loc *)
  tokens : Tok.t list Lazy.t;
}
[@@deriving show, eq]

(* A dependency in a manifest may have a version range like >=1.0.0, and they
 * are *direct* by definition Contains only an unparsed string for it's
 * package_version_constraint because we never actually use it for anything,
 * so parsing it is pointless.
 *)
type manifest_dependency = {
  package_name : string;
  package_version_constraint_string : string;
  ecosystem : SCA_ecosystem.t;
  loc : Tok.location * Tok.location;
  tokens : Tok.t list Lazy.t;
}
[@@deriving show, eq]
