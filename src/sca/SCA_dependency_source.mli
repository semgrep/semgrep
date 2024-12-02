type t =
  | ManifestOnly of Manifest.t
  | LockfileOnly of Lockfile.t
  | ManifestAndLockfile of Manifest.t * Lockfile.t
      (** A source to resolve dependencies from. Can be either a lockfile or a
          manifest, or both. *)

val dependency_source_of_semgrep_output :
  Semgrep_output_v1_t.dependency_source -> t
