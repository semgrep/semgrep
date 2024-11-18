type t = Semgrep_output_v1_j.manifest_kind [@@deriving show, eq, yojson]

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

(* For use in Input_to_core.atd *)
let wrap = of_string
let unwrap = Semgrep_output_v1_j.show_manifest_kind
