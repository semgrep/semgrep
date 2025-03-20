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
(* For the origin of the lockfile see SCA_dependency_source.ml
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = Out.lockfile [@@deriving show]
type kind = Out.lockfile_kind [@@deriving show, eq]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_lockfile kind (path : Fpath.t) : t = { path; kind }

(* coupling: if you need to add a case here, you probably need to also
 * extend of_string() above
 *)
let kind_to_ecosystem_opt : kind -> Semgrep_output_v1_t.ecosystem option =
  function
  | PipRequirementsTxt -> Some `Pypi
  | PoetryLock -> Some `Pypi
  | PipfileLock -> Some `Pypi
  | NpmPackageLockJson -> Some `Npm
  | YarnLock -> Some `Npm
  | PnpmLock -> Some `Npm
  | GemfileLock -> Some `Gem
  | GoMod -> Some `Gomod
  | CargoLock -> Some `Cargo
  | MavenDepTree -> Some `Maven
  | GradleLockfile -> Some `Maven
  | ComposerLock -> Some `Composer
  | NugetPackagesLockJson -> Some `Nuget
  | PubspecLock -> Some `Pub
  | SwiftPackageResolved -> Some `SwiftPM
  | MixLock -> Some `Hex
  | UvLock -> Some `Pypi
  | ConanLock -> None
  | PodfileLock -> Some `Cocoapods
  | OpamLocked -> Some `Opam

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
