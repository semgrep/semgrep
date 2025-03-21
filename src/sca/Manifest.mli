type kind = Semgrep_output_v1_j.manifest_kind =
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

type t = Semgrep_output_v1_t.manifest = { kind : kind; path : Fpath.t }
[@@deriving show]
(** A manifest file to be used during matching. See also
    {!Dependency_source_xtarget.manifest}, which also has the contents. *)

val mk_manifest : kind -> Fpath.t -> t

val kind_to_ecosystem_opt : kind -> Semgrep_output_v1_j.ecosystem option
(** Maps a manifest kind to its corresponding package ecosystem.

    A manifest (e.g. pyproject.toml, package.json) belongs to a specific
    package ecosystem (e.g. Poetry, NPM).

    If the manifest kind has a supported ecosystem, return [Some ecosystem].
    Otherwise, return [None], which means we don't have an ecosystem for the
    given manifest kind. *)

(* Try to infer the kind of a manifest based on its file name (e.g.,
 * package.json -> Npm). Will raise Failure for unknown manifest filename.
 * coupling: Match_subprojects.ml
 * This is used just by `semgrep show dump-lockfile` right now.
 *)
val kind_of_filename_exn : Fpath.t -> kind
