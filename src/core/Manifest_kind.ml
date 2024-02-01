type t = PackageJson [@@deriving show, eq]

let to_ecosystem : t -> Semgrep_output_v1_t.ecosystem = function
  | PackageJson -> `Npm

let supported_manifests = String.concat "," [ "package.json" ]

let unsupported_manifest_message (manifest_s : string) =
  Common.spf "unsupported manifest: %s; supported manifest types are: %s"
    manifest_s supported_manifests

let of_string = function
  | "PackageJson" -> PackageJson
  | s -> failwith (unsupported_manifest_message s)

let wrap = of_string
let unwrap = show

let of_lockfile_kind = function
  | Lockfile_kind.PackageLockJsonV3 -> PackageJson
