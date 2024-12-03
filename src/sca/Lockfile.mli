type t = Semgrep_output_v1_t.lockfile [@@deriving show]

val mk_lockfile : Lockfile_kind.t -> Fpath.t -> t
(** A lockfile to be used during matching. See also {!Lockfile_xtarget.t}, an
    augmented version with the contents of the lockfile. *)
