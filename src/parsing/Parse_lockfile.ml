let parse_lockfile :
    Lockfile.kind ->
    Lockfile_xtarget.manifest option ->
    Fpath.t ->
    SCA_dependency.t list = function
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
  | MixLock
  | UvLock
  | ConanLock ->
      fun _ _ -> []

let parse_manifest :
    Manifest.kind -> Fpath.t -> SCA_dependency.manifest_dependency list =
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
  | `PyprojectToml
  | `ConanFilePy
  | `ConanFileTxt ->
      fun _ -> []
