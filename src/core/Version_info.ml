(*
   Details about the Semgrep version for this build.

   The Semgrep version string comes from a generated file.
*)

type t = Semver.t

let pp fmt x = Format.pp_print_string fmt (Semver.to_string x)
let compare = Semver.compare
let of_string = Semver.of_string
let to_string = Semver.to_string
let string = Version.version

let version =
  match Semver.of_string string with
  | Some x -> x
  | None ->
      failwith
        ("Cannot parse the Semgrep version string found in the Version module: "
       ^ Version.version)

let major, minor, patch = version
