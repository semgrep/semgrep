type t = Semgrep_output_v1_j.lockfile_kind [@@deriving show, eq, yojson]

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

let supported_lockfiles = String.concat "," [ "package-lock.json v3" ]

let unsupported_lockfile_message (lockfile_s : string) =
  Common.spf "unsupported lockfile: %s; supported lockfile tags are: %s"
    lockfile_s supported_lockfiles

let of_string : string -> t = function
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
  | s -> failwith (unsupported_lockfile_message s)

let wrap = of_string
let unwrap = show
