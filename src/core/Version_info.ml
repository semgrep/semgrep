(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
