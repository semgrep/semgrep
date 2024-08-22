(*
   Details about the Semgrep version for this build.

   The Semgrep version string comes from a generated file.

   TODO: merge with Version.ml instead?
*)

let version =
  match Semver.of_string Version.version with
  | Some x -> x
  | None ->
      failwith
        ("Cannot parse the Semgrep version string found in the Version module: "
       ^ Version.version)

let major, minor, patch = version
