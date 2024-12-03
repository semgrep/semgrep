type t = Semgrep_output_v1_t.manifest [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

type kind = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq]

val mk_manifest : kind -> Fpath.t -> t
val kind_to_ecosystem : kind -> Semgrep_output_v1_j.ecosystem
