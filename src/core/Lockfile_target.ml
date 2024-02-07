type t = {
  source : Source.t;
      (** The source of the data as is relevant to the user. This could be, e.g., a
      relative (from the project root) path to a file, a git object and
      associated information, or anything else a Source.t can designate.

      This should be used when reporting a location to the user. *)
  file : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could be
      the same as the source, if the source is a path to a regular file (or an
      absolute path to the same), or it could be a tempfile. This should be
      used to obtain the contents of the target, but not for reporting to the
      user, other than possibly for debugging purposes. *)
  kind : Lockfile_kind.t;
      (** The type of lockfile this is. Analogous to analyzer for a source code
        target. *)
  manifest : manifest_target option;
      (** Optionally, a manifest file associated with this lockfile. *)
  lazy_content : string lazy_t;
      (** The contents of the lockfile, as a string. *)
  lazy_ast_and_errors : Dependency.t list lazy_t;
      (** The parsed contents of the lockfile, comprising the list of specified
          dependencies and their versions. *)
}
(** A lockfile to be scanned by a supply chain rule.

   A lockfile is a file which specifies exact versions of every dependency,
   including transitive dependencies, which belong to the "locked" package.
   Typically this is generated from a more general {{!manifest_target}
   "manifest" file}.

   Examples of lockfiles include [package-lock.json] (javascript/npm) or
   [Cargo.lock] (rust/cargo).

   Can be attached to a {{!Xtarget.t} code target}, or it can be a standalone
   target by itself.
*)

and manifest_target = {
  source : Source.t;
      (** The source of the data as is relevant to the user. This could be, e.g., a
      relative (from the project root) path to a file, a git object and
      associated information, or anything else a Source.t can designate.

      This should be used when reporting a location to the user. *)
  file : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could be
      the same as the source, if the source is a path to a regular file (or an
      absolute path to the same), or it could be a tempfile. This should be
      used to obtain the contents of the target, but not for reporting to the
      user, other than possibly for debugging purposes. *)
  kind : Manifest_kind.t;
      (** The type of manifest this is. Analogous to analyzer for a source code
        target. *)
  lazy_content : string lazy_t;
      (** The contents of the manifest, as a string. *)
  lazy_ast_and_errors : Dependency.manifest_dependency list lazy_t;
}
(** A manifest file to be scanned. This can only ever be attached to a
   {{!t}lockfile target}.

   A manifest file contains the list of developer-specified (generally
   handwritten) dependencies. This only includes {e direct} dependencies.
   Frequently this may include a "flexible" version specification (e.g., in npm
   syntax, [^X.Y.Z], which specifies any version [X.Y'.Z] where [Y'] {m \ge}
   [Y]).

   We currently use this in the parsing of some lockfiles, to determine which
   dependencies are direct and which are transitive. Future uses may include
   determining if a dependency is of a certain type, such as
   {{https://github.com/npm/npm/blob/2e3776bf5676bc24fec6239a3420f377fe98acde/doc/files/package.json.md#devdependencies}
   only for development}.

   Examples of manifest files include [package.json] (javascript/npm) or
   [Cargo.toml] (rust/cargo).
*)
