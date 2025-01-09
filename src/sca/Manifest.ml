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
(* Manifest kind and path (e.g., a package.json for the NPM ecosystem).
 * Note that this is different from a lock file (e.g., a package-lock.json for
 * NPM).
 *
 * Like for Lockfile.ml, this module is just to designate a manifest file.
 * The actual parsed content of a manifest is defined in SCA_dependency.ml
 * (and Lockfile_xtarget.ml)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* old: used to be path : Target.path but no need complex origin for manifest*)
type t = Semgrep_output_v1_t.manifest [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

type kind = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_manifest (kind : kind) (path : Fpath.t) : t = { path; kind }

let kind_to_ecosystem_opt : kind -> Semgrep_output_v1_t.ecosystem option =
  function
  | `RequirementsIn
  | `Pipfile
  | `PyprojectToml ->
      Some `Pypi
  | `PackageJson -> Some `Npm
  | `Gemfile -> Some `Gem
  | `GoMod -> Some `Gomod
  | `CargoToml -> Some `Cargo
  | `PomXml
  | `BuildGradle
  | `SettingsGradle ->
      Some `Maven
  | `ComposerJson -> Some `Composer
  | `NugetManifestJson
  | `Csproj ->
      Some `Nuget
  | `PubspecYaml -> Some `Pub
  | `PackageSwift -> Some `SwiftPM
  | `MixExs -> Some `Mix
  | `ConanFilePy
  | `ConanFileTxt ->
      None
  | `Podfile -> failwith "TODO"
