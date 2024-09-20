type t =
  | PackageJson
    (* An NPM package.json manifest file - https://docs.npmjs.com/cli/v10/configuring-npm/package-json *)
  | PomXml
    (* A Maven pom.xml manifest file - https://maven.apache.org/guides/introduction/introduction-to-the-pom.html *)
  | BuildGradle
    (* A Gradle build.gradle build file  - https://docs.gradle.org/current/userguide/build_file_basics.html *)
[@@deriving show, eq, yojson]

let to_ecosystem : t -> Semgrep_output_v1_t.ecosystem = function
  | PackageJson -> `Npm
  | PomXml -> `Maven
  | BuildGradle -> `Maven

let of_string s =
  let unsupported_manifest_message (manifest_s : string) =
    let supported_manifests =
      String.concat "," [ "package.json"; "pom.xml"; "build.gradle" ]
    in

    Common.spf "unsupported manifest: %s; supported manifest types are: %s"
      manifest_s supported_manifests
  in

  match s with
  | "PackageJson" -> PackageJson
  | "PomXml" -> PomXml
  | "BuildGradle" -> BuildGradle
  | s -> failwith (unsupported_manifest_message s)

(* For use in Input_to_core.atd *)
let wrap = of_string
let unwrap = show

let of_lockfile_kind = function
  | Lockfile_kind.PackageLockJsonV3 -> PackageJson
