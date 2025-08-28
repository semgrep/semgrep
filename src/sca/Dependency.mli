(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Complete information about a package dependency and especially its location
   in a lockfile.

   This is similar to found_dependency in semgrep_output_v1.atd
   (hence the to_found_dependency() function further below)
*)

type kind = Semgrep_output_v1_t.dependency_kind =
  (* we depend directly on the 3rd-party library mentioned in the lockfile
   * (e.g., use of log4j library and concrete calls to log4j in 1st-party code).
   * log4j must be declared as a direct dependency in the manifest file.
   *)
  | Direct
  (* we depend indirectly (transitively) on the 3rd-party library
   * (e.g., if we use lodash which itself uses internally log4j then
   * lodash is a Direct dependency and log4j a Transitive one)
   *)
  | Transitive
  (* If there is insufficient information to determine the transitivity,
   * such as a requirements.txt file without a requirements.in manifest,
   * we leave it Unknown.
   *)
  | Unknown
[@@deriving eq, ord, show]

(* TODO: add other kinds of hashes as needed. The ATD interface is untyped
   wrt hash names.

   Assumption: multiple hashes of the same kind are supported due to packages
   being recompressed with different options as on GitHub, resulting
   in checksums changing once in a while.
*)
type hashes = { sha1 : Hex_.t list; sha256 : Hex_.t list; sha512 : Hex_.t list }
[@@deriving eq, ord, show]

type t = {
  package : Package.t;
  (* note that this is the parsed version of package.version *)
  version : SCA_version.t;
  ecosystem : Ecosystem.t;
  allowed_hashes : hashes;
  transitivity : kind;
  url : Uri.t option;
  (* the location of the dependency source code, if it exists
   * (used by the transitive reachability analysis)
   *
   * Note that this is a list of paths because it is not always possible to
   * determine a single directory containing a package's source code. Python's
   * 'setuptoops' package provides on example: it unpacks into two directories
   * in the `site-packages` directory, both of which contain source code that we
   * need to scan.
   *
   * TODO? could switch to Rpath.t here? or invent a new Pkgpath.t?
   *)
  downloaded_source_paths : Fpath.t list option;
  (* start and end token location of the package entry in the lockfile
   * (e.g., '{' and '}' around a package entry in a package-lock.json file).
   *)
  loc : Tok.location * Tok.location;
}
[@@deriving eq, ord, show]

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
  ecosystem : Ecosystem.t;
  (* start and end token location of the package entry in the manifest *)
  loc : Tok.location * Tok.location;
}
[@@deriving show, eq]

val compare_source_location : t -> t -> int

val compare : t -> t -> int
(** same as compare_source_location *)

val dependency_kind : Package.t -> Package.name list option -> kind
(** Determines the dependency kind (direct, transitive, etc.) based on the given package
    and an optional list of direct dependency names. If the package's name is in the list
    of direct dependencies, it's considered a direct dependency; otherwise, it's treated
    as transitive. *)

val empty_hashes : hashes

val sha1 : Hex_.t -> hashes
(** Multiple hashes are supported but for sources that provide only one
    hash, the following functions are handy. *)

val sha256 : Hex_.t -> hashes
val sha512 : Hex_.t -> hashes

val to_found_dependency :
  ?lockfile_path:Fpath.t ->
  ?manifest_path:Fpath.t ->
  t ->
  t list option ->
  Semgrep_output_v1_t.found_dependency
