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
(* Manifest kind *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq]

(*****************************************************************************)
(* Misc  *)
(*****************************************************************************)
let to_ecosystem : t -> Semgrep_output_v1_t.ecosystem = function
  | `RequirementsIn -> `Pypi
  | `PackageJson -> `Npm
  | `Gemfile -> `Gem
  | `GoMod -> `Gomod
  | `CargoToml -> `Cargo
  | `PomXml -> `Maven
  | `BuildGradle -> `Maven
  | `SettingsGradle -> `Maven
  | `ComposerJson -> `Composer
  | `NugetManifestJson -> `Nuget
  | `PubspecYaml -> `Pub
  | `PackageSwift -> `SwiftPM
  | `MixExs -> `Mix
  | `Pipfile -> `Pypi
  | `PyprojectToml -> `Pypi
  | `ConanFilePy
  | `ConanFileTxt ->
      failwith "TODO"
