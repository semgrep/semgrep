(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Anything that can help deprecate old features and teach users how to
 * migrate to the new way to do things.
 *)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

(* warn people if they still rely on the deprecated .semgrep.yml
 * or .semgrep/ rules folder (except if it's the usual ~/.semgrep).
 *)
let abort_if_use_of_legacy_dot_semgrep_yml () =
  if
    Sys_.file_exists ".semgrep.yml"
    || Sys_.file_exists ".semgrep"
       && not (Sys_.file_exists ".semgrep/settings.yml")
  then (
    flush stdout;
    Logs.err (fun m ->
        m
          "The implicit use of .semgrep.yml (or .semgrep/) has been deprecated \
           in Semgrep 1.38.0.\n\
           Please use an explicit --config .semgrep.yml (or --config .semgrep/)");
    Error.exit_code_exn (Exit_code.fatal ~__LOC__))
