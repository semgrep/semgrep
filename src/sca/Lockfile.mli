type t = {
  path : Fpath_.t;
  kind : Lockfile_kind.t;
      (** The type of lockfile this is. Analogous to analyzer for a source code
          target. *)
}
[@@deriving show, yojson]

(** A lockfile to be used during matching. See also {!Lockfile_xtarget.t}, an
    augmented version with the contents of the lockfile. *)

val mk_lockfile : Lockfile_kind.t -> Fpath.t -> t
