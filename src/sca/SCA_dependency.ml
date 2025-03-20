(* Ben Kettle
 *
 * Copyright (c) 2024, Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Complete information about an SCA dependency and especially its location
 * in a lockfile.
 *
 * This is similar to found_dependency in semgrep_output_v1.atd
 * (hence the to_found_dependency() function further below)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  package : Package.t;
  (* note that this is the parsed version of package.version *)
  version : SCA_version.t;
  ecosystem : SCA_ecosystem.t;
  transitivity : SCA_transitivity.t;
  url : Uri.t option;
  (* the location of the dependency source code, if it exists
   * (used by the transitive reachability analysis)
   *)
  downloaded_source_path : Fpath.t option;
  (* start and end token location of the package entry in the lockfile
   * (e.g., '{' and '}' around a package entry in a package-lock.json file).
   *)
  loc : Tok.location * Tok.location;
}
[@@deriving show, eq]

(* Note that package entries in a manifest are *direct* by definition, which
 * is why there is no need for a 'transitive' field below.
 *)
type manifest_dependency = {
  package_name : Package.name;
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

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let to_found_dependency ?(lockfile_path : Fpath.t option)
    ?(manifest_path : Fpath.t option) (dep : t) (children : t list option) :
    Out.found_dependency =
  Out.
    {
      package = dep.package.name;
      version = dep.package.version;
      ecosystem = dep.ecosystem;
      allowed_hashes = [];
      resolved_url = Option.map Uri.to_string dep.url;
      transitivity = dep.transitivity;
      lockfile_path =
        (if Option.is_none lockfile_path then Some (fst dep.loc).pos.file
         else lockfile_path);
      manifest_path;
      line_number = Some (fst dep.loc).pos.line;
      git_ref = None;
      children =
        children
        |> Option.map
             (List_.map (fun (dep : t) ->
                  Out.
                    {
                      package = dep.package.name;
                      version = dep.package.version;
                    }));
    }
