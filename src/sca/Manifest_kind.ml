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
(* Helpers around semgrep_output_v1.atd and Input_to_core.atd manifest.
 *
 * TODO: use the same technique than in Product.ml and define lockfile_kind in
 * both .atd and make sure they are equal via a compile-time check.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Semgrep_output_v1_j.manifest_kind (* = Input_to_core.manifest_kind *)
[@@deriving show, eq]

(*****************************************************************************)
(* ATD string wrap  *)
(*****************************************************************************)

let of_string s =
  let unsupported_manifest_message (manifest_s : string) =
    let supported_manifests =
      String.concat "," [ "package.json"; "pom.xml"; "build.gradle" ]
    in
    Common.spf "unsupported manifest: %s; supported manifest types are: %s"
      manifest_s supported_manifests
  in
  match s with
  | "RequirementsIn" -> `RequirementsIn
  | "PackageJson" -> `PackageJson
  | "Gemfile" -> `Gemfile
  | "GoMod" -> `GoMod
  | "CargoToml" -> `CargoToml
  | "PomXml" -> `PomXml
  | "BuildGradle" -> `BuildGradle
  | "ComposerJson" -> `ComposerJson
  | "NugetManifestJson" -> `NugetManifestJson
  | "PubspecYaml" -> `PubspecYaml
  | "PackageSwift" -> `PackageSwift
  | "MixExs" -> `MixExs
  | "Pipfile" -> `Pipfile
  | "PyprojectToml" -> `PyprojectToml
  | s -> failwith (unsupported_manifest_message s)

(* for the 'string wrap' in Input_to_core.atd *)
let unwrap = show
let wrap = of_string

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
