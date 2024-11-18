module In = Input_to_core_t

let parse_lockfile :
    In.lockfile_kind ->
    Lockfile_xtarget.manifest option ->
    Fpath.t ->
    Dependency.t list = function
  (* TODO: add parsers, guard behind semgrep-pro  *)
  | PipRequirementsTxt
  | PoetryLock
  | PipfileLock
  | NpmPackageLockJson
  | YarnLock
  | PnpmLock
  | GemfileLock
  | GoMod
  | CargoLock
  | MavenDepTree
  | GradleLockfile
  | ComposerLock
  | NugetPackagesLockJson
  | PubspecLock
  | SwiftPackageResolved
  | MixLock ->
      fun _ _ -> []

let parse_manifest :
    In.manifest_kind -> Fpath.t -> Dependency.manifest_dependency list =
  function
  (* TODO: add parsers, guard behind semgrep-pro  *)
  | `RequirementsIn
  | `PackageJson
  | `Gemfile
  | `GoMod
  | `CargoToml
  | `PomXml
  | `BuildGradle
  | `SettingsGradle
  | `ComposerJson
  | `NugetManifestJson
  | `PubspecYaml
  | `PackageSwift
  | `MixExs
  | `Pipfile
  | `PyprojectToml ->
      fun _ -> []
