(* Cooper Pierce
 *
 * Copyright (c) Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(** Types for describing targets.

    See also {!Input_to_core_t}, which has a similar set of types used when
    pysemgrep generates targets that have slightly less information (e.g.,
    these types have expanded information about the targets' locations). *)

(** The location of a Semgrep target. This contains all of the target-specific
    details needed to be able to determine how to scan a target, e.g.,

    {ul
      {- What products should we select rules from?}
      {- Where can we get the contents of the target?}
      {- What language should we analyze the target as?}
    }

    However, it does not contain the actual contents (parsed or otherwise) of
    the target itself.
 *)
type t = Code of code | Lockfile of lockfile [@@deriving show]

and code = {
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
          generating code targets. *)
}
[@@deriving show]
(** A "normal" semgrep target, comprising source code (or, for
   regex/generic, arbitrary text data) to be executed. See also {!Xtarget.t},
   an augmented version which also has the contents. *)

and lockfile = {
  path : path;
  kind : Lockfile_kind.t;
      (** The type of lockfile this is. Analogous to analyzer for a source code
          target. *)
  manifest : manifest option;
      (** Optionally, a manifest file associated with this lockfile. *)
}
[@@deriving show]
(** A lockfile to be used during matching. See also {!Lockfile_xtarget.t}, an
    augmented version with the contents of the lockfile. *)

and manifest = {
  path : path;
  kind : Manifest_kind.t;
      (** The type of manifest this is. Analogous to analyzer for a source code
        target. *)
}
[@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest_target}, which also has the contents. *)

and path = {
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
[@@deriving show]
(** Information about where a target from for both the purpose of
   {ul
    {- informing the user: [origin]}
    {- obtaining the contents: [internal_path_to_content]}
  } *)

(* deriving eq appears to not work on mutual definitions with and. See also
   <https://github.com/ocaml-ppx/ppx_deriving/issues/272>, for deriving make,
   which may be relevant. Therefore, we just define it here. *)
val equal_path : path -> path -> bool

val mk_code :
  ?lockfile:lockfile ->
  Xlang.t ->
  Semgrep_output_v1_t.product list ->
  Origin.t ->
  code
(** [mk_code analyzer products origin] is the target
      location for a source code target originating from [origin] to be
      analyzed with [analyzer] for [products]. If [lockfile] is specified then
      it shall be used as the associated lockfile if dependency patterns are
      to be ran.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

val mk_lockfile : ?manifest:manifest -> Lockfile_kind.t -> Origin.t -> lockfile
(** [mk_lockfile k origin] is the target
      location for a lockfile target originating from [origin] of kind [k].
      If [manifest] is specified, it shall be used as the associated manifest.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

val mk_manifest : Manifest_kind.t -> Origin.t -> manifest
(** [mk_manifest k origin] is the target
      location for a manifest target originating from [origin] of kind [k].

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of origins, such as generating a tempfile.
 *)

val internal_path_to_content : t -> Fpath.t
(** [internal_path_to_content target] is the path to a file containing the
    contents of [target]. *)

val origin : t -> Origin.t
(** [origin target] is the user-reportable origin of [target]. *)
