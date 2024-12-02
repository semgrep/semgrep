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
(* Helpers around semgrep_output_v1.atd and Input_to_core.atd for lockfiles.
 *
 * LATER: once osemgrep is finished, we could get rid of Input_to_core.atd
 * and remove the need for those ATD 'string wrap' helpers. This is partly
 * because ATD does not support modules right now and so we can't reuse
 * the lockfile types defined in semgrep_output_v1.atd in Input_to_core.atd
 * and have to abuse 'string wrap'.
 *
 * TODO: use the same technique than in Product.ml and define lockfile_kind in
 * both .atd and make sure they are equal via a compile-time check.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Semgrep_output_v1_t.lockfile_kind (* = Input_to_core.lockfile_kind *)
[@@deriving show, eq, yojson]

(*****************************************************************************)
(* ATD string wrap  *)
(*****************************************************************************)

(* TODO: alt: now that lockfile_kind is defined in semgrep_output_v1.atd,
 * we could reuse its of_string function or better use the same tech
 * than in Product.ml as mentioned above.
 *)
let of_string (s : string) : t =
  let supported_lockfiles = String.concat "," [ "package-lock.json v3" ] in
  let unsupported_lockfile_message (lockfile_s : string) =
    Common.spf "unsupported lockfile: %s; supported lockfile tags are: %s"
      lockfile_s supported_lockfiles
  in
  match s with
  | "PipRequirementsTxt" -> PipRequirementsTxt
  | "PoetryLock" -> PoetryLock
  | "PipfileLock" -> PipfileLock
  | "NpmPackageLockJson" -> NpmPackageLockJson
  | "YarnLock" -> YarnLock
  | "PnpmLock" -> PnpmLock
  | "GemfileLock" -> GemfileLock
  | "GoMod" -> GoMod
  | "CargoLock" -> CargoLock
  | "MavenDepTree" -> MavenDepTree
  | "GradleLockfile" -> GradleLockfile
  | "ComposerLock" -> ComposerLock
  | "NugetPackagesLockJson" -> NugetPackagesLockJson
  | "PubspecLock" -> PubspecLock
  | "SwiftPackageResolved" -> SwiftPackageResolved
  | "MixLock" -> MixLock
  | "UvLock" -> UvLock
  | "ConanLock" -> ConanLock
  | s -> failwith (unsupported_lockfile_message s)

(* for the 'string wrap' in Input_to_core.atd *)
let unwrap = show
let wrap = of_string

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
  | UvLock
  | ConanLock ->
      failwith "TODO"
