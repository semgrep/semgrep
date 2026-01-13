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

val command_exists : string -> unit -> bool
(** Call the system command [which] or [where.exe] to determine
    if the given command name is available.

    The extra unit argument is to facilitate partial application such that
    we'd write [command_exists "foo"] to produce a function of type
    [unit -> unit] that checks for the prerequisite.
*)

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

val skip_test_if_missing_prerequisites :
  prerequisite_exists:(string -> bool) -> string list -> Testo.t -> Testo.t
(** Rewrite a test to be skipped if the prerequisites, such as
    certain external commands, are not fulfilled.
    This results in the test being skipped with an explanation
    rather than missing mysteriously.

    The [prerequisite_exists] function should be produced with
    [detect_available_commands] or [check_prerequisites] so it can be fast
    when checking the same prerequisite repeatedly.
*)

val skip_tests_if_missing_prerequisites :
  prerequisite_exists:(string -> bool) ->
  string list ->
  Testo.t list ->
  Testo.t list
(** Same as [skip_test_if_missing_prerequisites] but applies to a list
    of tests instead just one test. *)

val run_command :
  ?expected_exit_code:int -> ?on_error:(unit -> unit) -> string list -> unit
(** Run an external command provided as a string list.
    The command is logged to stderr.
    Raise an exception if the command doesn't terminate with the
    expected exit code.

    @param expected_exit_code the expected exit code. Defaults to 0.
    @param on_error a function to call if the command doesn't exit as expected
*)
