(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*****************************************************************************)
(* Main type *)
(*****************************************************************************)

(** Represents the purpose or type of a file path being considered as a target.
    This is primarily used during target discovery (e.g., in Find_targets) to apply
    different filtering logic based on the file's nature.
*)

type t =
  | Source_file
      (** A regular source code file intended for primary analysis. *)
  | Dependency_source_file
      (** A file related to dependencies (e.g., lockfile, manifest). Often exempt from size limits. *)
  | Unknown
      (** Could not be confidently classified, or classification is not implemented/hooked. *)
[@@deriving show, eq]

val pp_debug : Format.formatter -> t -> unit
(** [pp_debug fmt t] pretty-prints [t] to [fmt]. *)

(* TODO: switch to Hook.t *)
val hook_classify_target : (Fpath.t -> t) option ref

val classify_target : Fpath.t -> t
(** [classify_target path] returns the target kind of [path].
    If semgrep pro is not loaded, this will always return [Source_file]. *)

val is_dependency_source_file : Fpath.t -> bool
(** [is_dependency_source_file path] returns true if [path] is a dependency source file.
    If semgrep pro is not loaded, this will always return [false]. *)
