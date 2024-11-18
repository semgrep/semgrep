(** Types for describing targets.

    See also {!Input_to_core_t}, which has a similar set of types used when
    pysemgrep generates targets that have slightly less information (e.g.,
    these types have expanded information about the targets' locations). *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type path = {
  origin : Origin.t;
      (** The origin of the data as is relevant to the user. This could be,
          e.g., a relative (from the project root) path to a file, a git object
          and associated information, or anything else a Origin.t can
          designate.

          This should be used when reporting a location to the user. *)
  internal_path_to_content : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could
          be the same as the origin, if the origin is a path to a regular file
          (or an absolute path to the same), or it could be a tempfile. This
          should be used to obtain the contents of the target, but not for
          reporting to the user, other than possibly for debugging purposes. *)
}
[@@deriving show, eq, yojson]
(** Information about where a target from for both the purpose of
   {ul
    {- informing the user: [origin]}
    {- obtaining the contents: [internal_path_to_content]}
  } *)

type manifest = {
  path : path;
  kind : Manifest_kind.t;
      (** The type of manifest this is. Analogous to analyzer for a source code
        target. *)
}
[@@deriving show, yojson]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

type lockfile = {
  path : path;
  kind : Lockfile_kind.t;
      (** The type of lockfile this is. Analogous to analyzer for a source code
          target. *)
}
[@@deriving show, yojson]
(** A lockfile to be used during matching. See also {!Lockfile_xtarget.t}, an
    augmented version with the contents of the lockfile. *)

type dependency_source =
  | ManifestOnly of manifest
  | LockfileOnly of lockfile
  | ManifestAndLockfile of manifest * lockfile
      (** A source to resolve dependencies from. Can be either a lockfile or a manifest, or both. *)

val pp_debug_lockfile : Format.formatter -> lockfile -> unit

type regular = {
  path : path;
  analyzer : Xlang.t;  (** The analyzer to use when scanning this target. *)
  products : Semgrep_output_v1_t.product list;
      (** The products which should scan this target. This is used for
          selecting the relevant set of rules. *)
  lockfile : lockfile option;
      (** Optional lockfile associated with this target.

          The association is namely that this target has its dependencies
          specified by the given lockfile. Core doesn't need to worry about
          determining these associations; rather, the target selection and
          generation process must resolve these connections as part of
          generating regular targets. *)
}
[@@deriving show, yojson]
(** A regular semgrep target, comprising source code (or, for
   regex/generic, arbitrary text data) to be executed. See also {!Xtarget.t},
   an augmented version which also has the contents. *)

val pp_debug_regular : Format.formatter -> regular -> unit

(** A Semgrep target. This contains all of the details needed to be able to
    determine how to scan a target, e.g.,

    {ul
      {- What products should we select rules from?}
      {- Where can we get the contents of the target?}
      {- What language should we analyze the target as?}
    }

    However, it does not contain the actual contents (parsed or otherwise) of
    the target itself. For that, see {!Xtarget.t} or {!Lockfile_xtarget}.
 *)
type t = Regular of regular | Lockfile of lockfile [@@deriving show, yojson]

val pp_debug : Format.formatter -> t -> unit

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

val mk_regular :
  ?lockfile:lockfile ->
  Xlang.t ->
  Semgrep_output_v1_t.product list ->
  Origin.t ->
  regular
(** [mk_regular analyzer products origin] is a {!regular} target
      originating from [origin] to be analyzed with [analyzer] for [products].
      If [lockfile] is specified then it shall be used as the associated
      lockfile if dependency patterns are to be ran.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

val mk_lockfile : Lockfile_kind.t -> Origin.t -> lockfile
(** [mk_lockfile k origin] is the a {!lockfile} target
      originating from [origin] of kind [k]. If [manifest] is specified, it
      shall be used as the associated manifest.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

val mk_manifest : Manifest_kind.t -> Origin.t -> manifest
(** [mk_manifest k origin] is a {!manifest} target
    originating from [origin] of kind [k].

    This function should be generally preferred over creating a record
    directly, since it can peform actions which may be required when creating a
    target from certain types of origins, such as generating a tempfile.
 *)

(* useful in tests *)
val mk_target : Xlang.t -> Fpath.t -> t

(*****************************************************************************)
(* Input_to_core -> Target *)
(*****************************************************************************)
val target_of_input_to_core : Input_to_core_t.target -> t

(*****************************************************************************)
(* Semgrep_output -> Target *)
(*****************************************************************************)

val dependency_source_of_semgrep_output :
  Semgrep_output_v1_t.dependency_source -> dependency_source

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val internal_path : t -> Fpath.t
(** [internal_path target] is the path to a file containing the
    contents of [target]. *)

val origin : t -> Origin.t
(** [origin target] is the user-reportable origin of [target]. *)

val analyzer : t -> Xlang.t option
