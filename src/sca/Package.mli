(*
 * Copyright (c) 2025, Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few private type for packages to avoid using 'string' everywhere

   Reminder about private types and private string types in particular:
   - 'type name = private string' prevents different string types from
     being interchangeable, unlike type aliases ('type name = string').
   - '(x :> string)' converts x to a regular string
   - constructing a value requires a helper function e.g. 'name "foo"'.
     Optionally, the constructor can perform some validation.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type name = private string [@@deriving eq, ord, show, yojson]
(** package name (e.g., "lodash")

   alt: we could use [type name = Name of string [@@unboxed]] instead
   of a private type. In both cases, the value is a plain string at runtime.
   One thing I prefer about private types is that the built-in [:>]
   cast operator makes it obvious that the conversion is a no-op
   and that it doesn't require a let-binding
   ([let Name name_str = name in] ...) or a function call
   ([Package.string_of_name name]) to extract the string.
*)

val name : string -> name
(** pure type conversion *)

val names : string list -> name list
(** pure type conversion *)

(* functional sets and maps operating on the name type *)
module Name_set : Set.S with type elt = name
module Name_map : Map.S with type key = name

type version = private string [@@deriving eq, ord, show, yojson]
(** package version (e.g., "1.1.0")
   See also SCA_version.t which is its parsed form. The syntax is dependent
   on the package manager.

   TODO: use SCA_version.t?
   Those strings usually appear in lockfiles.
   TODO: define a version-aware 'compare' function if it becomes important.
   For now, 'compare' is derived naively by ppx_deriving.ord.
 *)

val version : string -> version
(** pure type conversion *)

val name_and_version : string * string -> name * version
(** pure type conversion *)

type version_constraint = private string [@@deriving show, eq, yojson]
(** A formula specifying version constraints. Its syntax depends on the
    package manager.
    In yarn lockfiles and NPM manifests, it's something like "^1.1.0",
    "~1.1.0", "*", ">= 2.1.2 < 3.0.0", etc.
    This can also be a single version as in "1.1.0". Those strings
    usually appear in manifests.
*)

val version_constraint : string -> version_constraint
(** pure type conversion *)

(* See also SCA_dependency.t which specifies the ecosystem, URI,
   and location in a lockfile.
   This is mostly the same type that dependency_child in semgrep_output_v1.atd

   TODO: add support for aliases. In yarn/npm, an alias is the name
   given locally to a package. A typical use of aliases is when an application
   uses two different versions of the same library. A semgrep rule
   references the canonical (official) name of the library while the local
   name is what appears in the source code so we will need to maintain
   a mapping between the two.
*)
type t = {
  name : name;
  (* local_name : name option; *)
  version : version;
}
[@@deriving eq, ord, show]

val of_strings : name:string -> version:string -> t

val to_string : t -> string
(** Convert to a string of the form NAME@VERSION *)
