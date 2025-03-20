type t = Semgrep_output_v1_t.manifest [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Dependency_source_xtarget.manifest}, which also has the contents. *)

type kind = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq]

val mk_manifest : kind -> Fpath.t -> t

val kind_to_ecosystem_opt : kind -> Semgrep_output_v1_j.ecosystem option
(** Maps a manifest kind to its corresponding package ecosystem.

    A manifest (e.g. pyproject.toml, package.json) belongs to a specific
    package ecosystem (e.g. Poetry, NPM).

    If the manifest kind has a supported ecosystem, return [Some ecosystem].
    Otherwise, return [None], which means we don't have an ecosystem for the
    given manifest kind. *)

(* Try to infer the kind of a manifest based on its file name (e.g.,
 * package.json -> Npm). Will raise Failure for unknown manifest filename.
 * coupling: Match_subprojects.ml
 * This is used just by `semgrep show dump-lockfile` right now.
 *)
val kind_of_filename_exn : Fpath.t -> kind
