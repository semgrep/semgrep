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
open Common
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lockfile kind and path (e.g., package-lock.json in the NPM ecosystem).
 *
 * This module is just to designate a lockfile. The actual parsed content of a
 * lockfile is defined in SCA_dependency.ml (and Dependency_source_xtarget.ml)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type kind = Out.lockfile_kind =
  | PipRequirementsTxt
  | PoetryLock
  | PipfileLock
  | UvLock
  | NpmPackageLockJson
  | YarnLock
  | PnpmLock
  | GemfileLock
  | GoModLock
  | CargoLock
  | MavenDepTree (* Not a real lockfile *)
  | GradleLockfile
  | ComposerLock
  | NugetPackagesLockJson
  | PubspecLock
  | SwiftPackageResolved (* not a real lockfile *)
  | PodfileLock
  | MixLock
  | ConanLock
  | OpamLocked
[@@deriving show, eq]

(* For the origin of the lockfile see SCA_dependency_source.ml
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = Out.lockfile = { kind : kind; path : Fpath.t } [@@deriving show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_lockfile kind (path : Fpath.t) : t = { path; kind }

(* coupling: if you need to add a case here, you probably need to also
 * extend of_string() above
 *)
let kind_to_ecosystem_opt : kind -> Semgrep_output_v1_t.ecosystem option =
  function
  | PipRequirementsTxt -> Some Out.Pypi
  | PoetryLock -> Some Out.Pypi
  | PipfileLock -> Some Out.Pypi
  | NpmPackageLockJson -> Some Out.Npm
  | YarnLock -> Some Out.Npm
  | PnpmLock -> Some Out.Npm
  | GemfileLock -> Some Out.Gem
  | GoModLock -> Some Out.Gomod
  | CargoLock -> Some Out.Cargo
  | MavenDepTree -> Some Out.Maven
  | GradleLockfile -> Some Out.Maven
  | ComposerLock -> Some Out.Composer
  | NugetPackagesLockJson -> Some Out.Nuget
  | PubspecLock -> Some Out.Pub
  | SwiftPackageResolved -> Some Out.SwiftPM
  | MixLock -> Some Out.Hex
  | UvLock -> Some Out.Pypi
  | ConanLock -> None
  | PodfileLock -> Some Out.Cocoapods
  | OpamLocked -> Some Out.Opam

(* coupling: Match_subprojects.ml *)
let kind_of_filename_exn (file : Fpath.t) : kind =
  match Fpath.basename file with
  | "uv.lock" -> Out.UvLock
  | "yarn.lock" -> Out.YarnLock
  | "package-lock.json" -> Out.NpmPackageLockJson
  | "Pipfile.lock" -> Out.PipfileLock
  | "mix.lock" -> Out.MixLock
  | "requirements.txt" -> Out.PipRequirementsTxt
  | "Package.resolved" -> Out.SwiftPackageResolved
  | str -> failwith (spf "unrecognized lockfile: %s" str)
