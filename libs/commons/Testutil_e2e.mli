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

val detect_available_commands : string list -> string -> bool
(** [detect_available_commands command_names] is meant to be called once
    to return a function [is_cmd_available] that tells quickly whether a command
    is available. It should run only in a test program. The availability of
    each command is tested once with Unix [which] or equivalent and is
    then memoized.

    All the command names must be registered as part of the call to
    [detect_available]. [is_cmd_available] will raise a fatal exception
    if it's called on a command that wasn't registered. *)

val skip_tests_if_missing_commands :
  is_cmd_available:(string -> bool) ->
  string list ->
  Testo.t list ->
  Testo.t list
(** Rewrite a test suite to be skipped if the required external commands
    are not available.
    This results in the tests being listed as skipped with an explanation
    rather than missing mysteriously.

    The [is_cmd_available] function should be produced with
    [detect_available_commands].
*)
