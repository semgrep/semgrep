type t = Semgrep_output_v1_t.manifest [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

val mk_manifest : Manifest_kind.t -> Fpath.t -> t
