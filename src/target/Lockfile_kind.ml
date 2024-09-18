type t = PackageLockJsonV3 [@@deriving show, eq, yojson]

let to_ecosystem : t -> Semgrep_output_v1_t.ecosystem = function
  | PackageLockJsonV3 -> `Npm

let supported_lockfiles = String.concat "," [ "package-lock.json v3" ]

let unsupported_lockfile_message (lockfile_s : string) =
  Common.spf "unsupported lockfile: %s; supported lockfile tags are: %s"
    lockfile_s supported_lockfiles

let of_string = function
  | "PackageLockV3" -> PackageLockJsonV3
  | s -> failwith (unsupported_lockfile_message s)

let wrap = of_string
let unwrap = show
