(** Types for describing targets.

    See also semgrep_output_v1.atd which has a similar set of types used when
    pysemgrep generates targets that have slightly less information (e.g.,
    these types have expanded information about the targets' locations). *)

(*****************************************************************************)
(* Target path *)
(*****************************************************************************)

(* Yet another path (Fpath.t -> Origin.t -> Target.path) *)
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
[@@deriving show, eq]
(** Information about where a target from for both the purpose of
   {ul
    {- informing the user: [origin]}
    {- obtaining the contents: [internal_path_to_content]}
  } *)

(*****************************************************************************)
(* Regular target *)
(*****************************************************************************)

type regular = {
  path : path;
  analyzer : Analyzer.t;  (** The analyzer to use when scanning this target. *)
  products : Product.t list;
      (** The products which should scan this target. This is used for
          selecting the relevant set of rules. *)
  lockfile : Lockfile.t option;
      (** Optional lockfile associated with this target.

          The association is namely that this target has its dependencies
          specified by the given lockfile. Core doesn't need to worry about
          determining these associations; rather, the target selection and
          generation process must resolve these connections as part of
          generating regular targets. *)
}
(** A regular semgrep target, comprising source code (or, for
   regex/generic, arbitrary text data) to be executed. See also {!Xtarget.t},
   an augmented version which also has the contents. *)

(*****************************************************************************)
(* Main type *)
(*****************************************************************************)

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
type t = Regular of regular | Lockfile of Lockfile.t [@@deriving show]

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

val mk_regular :
  ?lockfile:Lockfile.t -> Analyzer.t -> Product.t list -> Origin.t -> regular
(** [mk_regular analyzer products origin] is a {!regular} target
      originating from [origin] to be analyzed with [analyzer] for [products].
      If [lockfile] is specified then it shall be used as the associated
      lockfile if dependency patterns are to be ran.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

(* useful in tests *)
val mk_target : Analyzer.t -> Fpath.t -> t

(*****************************************************************************)
(* Semgrep_output_v1.target -> Target.t *)
(*****************************************************************************)
val target_of_target : Semgrep_output_v1_t.target -> t

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val internal_path : t -> Fpath.t
(** [internal_path target] is the path to a file containing the
    contents of [target]. *)

val origin : t -> Origin.t
(** [origin target] is the user-reportable origin of [target]. *)

val analyzer : t -> Analyzer.t option

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

val pp_debug : Format.formatter -> t -> unit

(* used by Targeting_stats.ml for telemetry *)
val to_yojson : t -> Yojson.Safe.t

(* This is not implemented; we should not need it but we need a signature here
 * to typecheck the deriving yojson in other files.
 *)
val of_yojson : Yojson.Safe.t -> (t, string) result

(*****************************************************************************)
(* Helpers used internally but also in other files *)
(*****************************************************************************)
val path_of_origin : Origin.t -> path
