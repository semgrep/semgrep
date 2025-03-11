type t = {
  package_name : string;
  package_version : SCA_version.t;
  package_version_string : string;
  ecosystem : SCA_ecosystem.t;
  transitivity : SCA_transitivity.t;
  url : Uri.t option;
  (* start and end token location of the package entry in the lockfile
   * (e.g., '{' and '}' around a package entry in a package-lock.json file).
   *)
  loc : Tok.location * Tok.location;
  (* the location of the dependency source code, if it exists
   * (used by the transitive reachability analysis)
   *)
  downloaded_source_path : Fpath.t option;
}
[@@deriving show, eq]

(* Note that package entries in a manifest are *direct* by definition, which
 * is why there is no need for a 'transitive' field below.
 *)
type manifest_dependency = {
  package_name : string;
  (* A dependency in a manifest may have a version range like >=1.0.0.
   * It contains only an unparsed string for because we never actually use it
   * for anything, so parsing it is pointless.
   *)
  package_version_constraint_string : string;
  ecosystem : SCA_ecosystem.t;
  (* start and end token location of the package entry in the manifest *)
  loc : Tok.location * Tok.location;
}
[@@deriving show, eq]
