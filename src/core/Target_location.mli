(* Cooper Pierce
 *
 * Copyright (C) 2023 Semgrep Inc.
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

(** Types for describing target locations, i.e., everything about a target
    other than the actual contents. See also {!Input_to_core_t}, which has a
    similar set of types used when pysemgrep generates targets that have
    slightly less information (e.g., sources and files containing contents are
    not differentiated). *)

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
  source : Source.t;
      (** The source of the data as is relevant to the user. This could be, e.g., a
      relative (from the project root) path to a file, a git object and
      associated information, or anything else a Source.t can designate.

      This should be used when reporting a location to the user. *)
  file : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could be
      the same as the source, if the source is a path to a regular file (or an
      absolute path to the same), or it could be a tempfile. This should be
      used to obtain the contents of the target, but not for reporting to the
      user, other than possibly for debugging purposes. *)
  analyzer : Xlang.t;  (** The analyzer to use when scanning this target. *)
  products : Semgrep_output_v1_t.product list;
      (** The products which should scan this target. This is used for selecting
      the relevant set of rules. *)
  lockfile : lockfile option;
      (** Optional lockfile associated with this target.
      The association is namely that this target has its dependencies specified
      by the given lockfile. Core doesn't need to worry about determining these
      associations; rather, the target selection and generation process must
      resolve these connections as part of generating code targets. *)
}
[@@deriving show]
(** The location of a "normal" semgrep target, comprising source code (or for
   regex/generic, arbitrary text data) to be executed. See also {!Xtarget.t},
   the target this would designate. *)

(* A lockfile to be matched against dependency patterns. See also
 * {!Lockfile_target.t}, the target this would designate. *)
and lockfile = {
  source : Source.t;
      (** The source of the data as is relevant to the user. This could be, e.g., a
      relative (from the project root) path to a file, a git object and
      associated information, or anything else a Source.t can designate.

      This should be used when reporting a location to the user. *)
  file : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could be
      the same as the source, if the source is a path to a regular file (or an
      absolute path to the same), or it could be a tempfile. This should be
      used to obtain the contents of the target, but not for reporting to the
      user, other than possibly for debugging purposes. *)
  kind : Lockfile_kind.t;
      (** The type of lockfile this is. Analogous to analyzer for a source code
        target. *)
  manifest : manifest option;
      (** Optionally, a manifest file associated with this lockfile. *)
}
[@@deriving show]

and manifest = {
  source : Source.t;
      (** The source of the data as is relevant to the user. This could be, e.g., a
      relative (from the project root) path to a file, a git object and
      associated information, or anything else a Source.t can designate.

      This should be used when reporting a location to the user. *)
  file : Fpath.t;
      (** The path to a file which contains the data to be scanned. This could be
      the same as the source, if the source is a path to a regular file (or an
      absolute path to the same), or it could be a tempfile. This should be
      used to obtain the contents of the target, but not for reporting to the
      user, other than possibly for debugging purposes. *)
  kind : Manifest_kind.t;
      (** The type of manifest this is. Analogous to analyzer for a source code
        target. *)
}
[@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_target.manifest_target}, the target this would designate. *)

val code_of_source :
  ?lockfile:lockfile ->
  Xlang.t ->
  Semgrep_output_v1_t.product list ->
  Source.t ->
  code
(** [code_of_source analyzer products source] is the target
      location for a source code target originating from [source] to be
      analyzed with [analyzer] for the products [products]. If [lockfile] is
      specified then it shall be used as the associated lockfile if dependency
      patterns are to be ran.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of sources, such as generating a tempfile.
 *)

val lockfile_of_source :
  ?manifest:manifest -> Lockfile_kind.t -> Source.t -> lockfile
(** [lockfile_of_source kind source] is the target
      location for a lockfile target originating from [source] of kind [kind].
      If [manifest] is specified, it shall be used as the associated manifest.

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of sources, such as generating a tempfile.
 *)

val manifest_of_source : Manifest_kind.t -> Source.t -> manifest
(** [manifest_of_source kind source] is the target
      location for a manifest target originating from [source] of kind [kind].

      This function should be generally preferred over creating a record
      directly, since it can peform actions which may be required when creating
      a target from certain types of sources, such as generating a tempfile.
 *)

val file : t -> Fpath.t
(** [file target_loc] is the path to a file containing the contents of the
    target designated by [target_loc]. *)

val source : t -> Source.t
(** [source target_loc] is the user-reportable origin of the target location
    [target_loc]. *)
