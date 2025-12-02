(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   End-to-end testing utilities

   This includes:
   - checking for the availability of external commands
   - skipping a test if some command isn't available
   - running external commands
*)

val command_exists : string -> bool
(** Call the system command [which] or [where.exe] to determine
    if the given command name is available. *)

val check_prerequisites : (string * (unit -> bool)) list -> string -> bool
(** [check_prerequisites] takes a list of prerequisites in the form of
    named predicates, evaluates them, puts the results in a table,
    and returns a function [prerequisite_exists] for quick lookups.

    Each test has its own list of prerequisites. Providing the
    [prerequisite_exists] function to [skip_tests_if_missing_prerequisites]
    allows skipping the tests for which the prerequisites aren't fulfilled
    without causing errors.

    For example,
    [check_prerequisites ["npm", (fun () -> command_exists "npm")]]
    defines ["npm"] as a prerequisite that is fulfilled if the npm command
    is available on the system. A more advanced prerequisite would be
    the presence of a particular version of npm.
*)

val skip_tests_if_missing_prerequisites :
  prerequisite_exists:(string -> bool) ->
  string list ->
  Testo.t list ->
  Testo.t list
(** Rewrite a test suite to be skipped if the prerequisites, such as
    certain external commands, are not fulfilled.
    This results in the tests being listed as skipped with an explanation
    rather than missing mysteriously.

    The [prerequisite_exists] function should be produced with
    [detect_available_commands] or [check_prerequisites] so it can be fast
    when checking the same prerequisite repeatedly.
*)
