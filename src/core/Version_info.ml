(*
   Details about the Semgrep version for this build.

   The Semgrep version string comes from a generated file.
*)

type t = Semver.t

(* we don't have access to Semver code so can't add the deriving there
 * so we need to roll our own boilerplate imitating what
 * a simple [@@deriving show, ord] would do above.
 *)
let pp fmt x = Format.pp_print_string fmt (Semver.to_string x)
let compare = Semver.compare
let show = Semver.to_string
let to_string = Semver.to_string
let of_string = Semver.of_string

let version =
  match Semver.of_string Version.version with
  | Some x -> x
  | None ->
      failwith
        ("Cannot parse the Semgrep version string found in the Version module: "
       ^ Version.version)

let major, minor, patch = version
