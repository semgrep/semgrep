type t = Semgrep_output_v1_t.lockfile [@@deriving show]
type kind = Semgrep_output_v1_t.lockfile_kind [@@deriving show, eq]

val mk_lockfile : kind -> Fpath.t -> t
(** A lockfile to be used during matching.
    See also {!Dependency_source_xtarget.t}, an augmented version with the
    contents of the lockfile. *)

val kind_to_ecosystem_opt : kind -> Semgrep_output_v1_t.ecosystem option
(** Maps a lockfile kind to its corresponding package ecosystem.

    A lockfile (e.g. package-lock.json, Gemfile.lock) belongs to a specific
    package ecosystem (e.g. NPM, RubyGems).

    If the lockfile kind has a supported ecosystem, return [Some ecosystem].
    Otherwise, return [None], which means we don't have an ecosystem for the
    given lockfile kind. This typically means we've identified a lockfile
    format but don't yet support its ecosystem.

    This mapping is used in the SCA pattern matching process. Each SCA pattern
    specifies its target ecosystem in the `r2c-internal-project-depends-on` field:

    {[
      r2c-internal-project-depends-on:
        namespace: npm
        package: wrappy
        version: < 1.0.3
    ]}

    Used in SCA_scan.ml to filter which rules to apply given a lockfile. *)

(* Try to infer the kind of a lockfile based on its file name (e.g.,
 * package-lock.json -> Npm). Will raise Failure for unknown lockfile filename.
 * coupling: Match_subprojects.ml
 * This is used just by `semgrep show dump-lockfile` right now.
 *)
val kind_of_filename_exn : Fpath.t -> kind
