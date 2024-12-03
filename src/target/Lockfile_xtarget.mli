type t = {
  target : Lockfile.t;
  manifest : manifest option;
  lazy_content : string lazy_t;
      (** The contents of the lockfile, as a string. *)
  lazy_dependencies : SCA_dependency.t list lazy_t;
      (** The parsed contents of the lockfile, comprising the list of specified
          dependencies and their versions. *)
}
(** A lockfile to be scanned by a supply chain rule.

   A lockfile is a file which specifies exact versions of every dependency,
   including transitive dependencies, which belong to the "locked" package.
   Typically this is generated from a more general {{!manifest}
   "manifest" file}.

   Examples of lockfiles include [package-lock.json] (javascript/npm) or
   [Cargo.lock] (rust/cargo).

   Can be attached to a {{!Xtarget.t} code target}, or it can be a standalone
   target by itself.
*)

and manifest = {
  target : Manifest.t;
  lazy_content : string lazy_t;
      (** The contents of the manifest, as a string. *)
  lazy_dependencies : SCA_dependency.manifest_dependency list lazy_t;
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
   {{: https://github.com/npm/npm/blob/2e3776bf5676bc24fec6239a3420f377fe98acde/doc/files/package.json.md#devdependencies }
   only for development}.

   Examples of manifest files include [package.json] (javascript/npm) or
   [Cargo.toml] (rust/cargo).
*)

val resolve :
  (Manifest.kind -> Fpath.t -> SCA_dependency.manifest_dependency list) ->
  (Lockfile.kind -> manifest option -> Fpath.t -> SCA_dependency.t list) ->
  Lockfile.t ->
  Manifest.t option ->
  t
