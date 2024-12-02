type t = {
  path : Fpath.t;
  kind : Manifest_kind.t;
      (** The type of manifest this is. Analogous to analyzer for a source code
        target. *)
}
[@@deriving show, yojson]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

val mk_manifest : Manifest_kind.t -> Fpath.t -> t
