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

type kind = Out.manifest_kind =
  (* A Pip Requirements.in in file, which follows the format of requirements.txt
   * https://pip.pypa.io/en/stable/reference/requirements-file-format/ *)
  | RequirementsIn
  (* A setup.py file, which is a Python file that contains the setup configuration
   * for a Python project.
   * https://packaging.python.org/en/latest/guides/distributing-packages-using-setuptools/#setup-py *)
  | SetupPy
  (* An NPM package.json manifest file
   * https://docs.npmjs.com/cli/v10/configuring-npm/package-json *)
  | PackageJson
  (* A Ruby Gemfile manifest https://bundler.io/v2.5/man/gemfile.5.html *)
  | Gemfile
  (* go.modhttps://go.dev/doc/modules/gomod-ref *)
  | GoModManifest
  (* cargo.toml - https://doc.rust-lang.org/cargo/reference/manifest.html *)
  | CargoToml
  (* A Maven pom.xml manifest file
   * https://maven.apache.org/guides/introduction/introduction-to-the-pom.html *)
  | PomXml
  (* A Gradle build.gradle build file
   * https://docs.gradle.org/current/userguide/build_file_basics.html *)
  | BuildGradle
  (* A Gradle settings.gradle file
   * https://docs.gradle.org/current/userguide/settings_file_basics.html.
   * Multi-project builds are defined by settings.gradle rather than
   * build.gradle:
   * https://docs.gradle.org/current/userguide/multi_project_builds.html#multi_project_builds *)
  | SettingsGradle
  (* composer.json - https://getcomposer.org/doc/04-schema.md *)
  | ComposerJson
  (* manifest for nuget
   * could not find a reference; this may not actually exist *)
  | NugetManifestJson
  (* pubspec.yaml - https://dart.dev/tools/pub/pubspec *)
  | PubspecYaml
  (* Package.swift
   * https://docs.swift.org/package-manager/PackageDescription/PackageDescription.html *)
  | PackageSwift
  (* Podfile - https://guides.cocoapods.org/using/the-podfile.html *)
  | Podfile
  (* mix.exs
   * https://hexdocs.pm/elixir/introduction-to-mix.html#project-compilation *)
  | MixExs
  (* Pipfile - https://pipenv.pypa.io/en/latest/pipfile.html *)
  | Pipfile
  (* pyproject.toml
   * https://packaging.python.org/en/latest/guides/writing-pyproject-toml/ *)
  | PyprojectToml
  (* conanfile.txt
   * https://docs.conan.io/2.9/reference/conanfile_txt.html#conanfile-txt *)
  | ConanFileTxt
  (* conanfile.py - https://docs.conan.io/2.9/reference/conanfile.html *)
  | ConanFilePy
  (* .csproj - https://docs.microsoft.com/en-us/dotnet/core/tools/csproj *)
  | Csproj
  (* .opam - https://opam.ocaml.org/doc/Manual.html#Package-definitions *)
  | OpamFile
[@@deriving show, eq]

(* old: used to be path : Target.path but no need complex origin for manifest*)
type t = Out.manifest = { kind : kind; path : Fpath.t } [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Dependency_source_xtarget.ml}, which also has the contents. *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_manifest (kind : kind) (path : Fpath.t) : t = { path; kind }

let kind_to_ecosystem_opt (kind : kind) : Out.ecosystem option =
  match kind with
  | RequirementsIn
  | SetupPy
  | Pipfile
  | PyprojectToml ->
      Some Out.Pypi
  | PackageJson -> Some Out.Npm
  | Gemfile -> Some Out.Gem
  | GoModManifest -> Some Out.Gomod
  | CargoToml -> Some Out.Cargo
  | PomXml
  | BuildGradle
  | SettingsGradle ->
      Some Out.Maven
  | ComposerJson -> Some Out.Composer
  | NugetManifestJson
  | Csproj ->
      Some Out.Nuget
  | PubspecYaml -> Some Out.Pub
  | PackageSwift -> Some Out.SwiftPM
  | MixExs -> Some Out.Mix
  | ConanFilePy
  | ConanFileTxt ->
      None
  | Podfile -> Some Out.Cocoapods
  | OpamFile -> Some Out.Opam

(* coupling: Match_subprojects.ml *)
let kind_of_filename_exn (file : Fpath.t) : kind =
  match Fpath.basename file with
  | "mix.exs" -> Out.MixExs
  | str -> failwith (spf "unrecognized manifest: %s" str)
