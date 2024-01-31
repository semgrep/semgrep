type t = PackageLockJsonV3 [@@deriving show, eq]

let to_ecosystem = function
  | PackageLockJsonV3 -> Dependency.Npm

let supported_lockfiles = String.concat "," [ "package-lock.json v3" ]

let unsupported_lockfile_message (lockfile_s : string) =
  Common.spf "unsupported lockfile: %s; supported lockfile tags are: %s"
    lockfile_s supported_lockfiles

let of_string = function
  | "PackageLockV3" -> PackageLockJsonV3
  | s -> failwith (unsupported_lockfile_message s)

let wrap = of_string
let unwrap = show
