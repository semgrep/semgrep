type t = Semgrep_output_v1_t.lockfile [@@deriving show]
type kind = Semgrep_output_v1_t.lockfile_kind [@@deriving show, eq]

val mk_lockfile : kind -> Fpath.t -> t
(** A lockfile to be used during matching. See also {!Lockfile_xtarget.t}, an
    augmented version with the contents of the lockfile. *)

(* used in Core_scan.ml *)
val kind_to_ecosystem : kind -> Semgrep_output_v1_t.ecosystem
