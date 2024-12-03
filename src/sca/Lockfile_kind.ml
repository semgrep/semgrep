(* Matthew McQuaid
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
(* Lockfile kind *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Semgrep_output_v1_t.lockfile_kind [@@deriving show, eq]

(*****************************************************************************)
(* Misc  *)
(*****************************************************************************)

(* coupling: if you need to add a case here, you probably need to also
 * extend of_string() above
 *)
let to_ecosystem : t -> Semgrep_output_v1_t.ecosystem = function
  | PipRequirementsTxt -> `Pypi
  | PoetryLock -> `Pypi
  | PipfileLock -> `Pypi
  | UvLock -> `Pypi
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
  | ConanLock -> failwith "Conan is not supported"
