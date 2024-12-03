(* Matthew McQuaid, Cooper Pierce
 *
 * Copyright (c) 2024, Semgrep Inc.
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
(* Lockfile kind and path (e.g., package-lock.json in the NPM ecosystem).
 *
 * This module is just to designate a lockfile. The actual parsed content
 * of a lockfile is defined in SCA_dependency.ml (and Lockfile_xtarget.ml)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO? add a manifest option in it? to know the origin of the lockfile?
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = Semgrep_output_v1_t.lockfile [@@deriving show]
type kind = Semgrep_output_v1_t.lockfile_kind [@@deriving show, eq]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_lockfile kind (path : Fpath.t) : t = { path; kind }

(* coupling: if you need to add a case here, you probably need to also
 * extend of_string() above
 *)
let kind_to_ecosystem : kind -> Semgrep_output_v1_t.ecosystem = function
  | PipRequirementsTxt -> `Pypi
  | PoetryLock -> `Pypi
  | PipfileLock -> `Pypi
  | NpmPackageLockJson -> `Npm
  | YarnLock -> `Npm
  | PnpmLock -> `Npm
  | GemfileLock -> `Gem
  | GoMod -> `Gomod
  | CargoLock -> `Cargo
  | MavenDepTree -> `Maven
  | GradleLockfile -> `Maven
  | ComposerLock -> `Composer
  | NugetPackagesLockJson -> `Nuget
  | PubspecLock -> `Pub
  | SwiftPackageResolved -> `SwiftPM
  | MixLock -> `Hex
  | UvLock -> `Pypi
  | ConanLock -> failwith "Conan not supported"
