(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Exit codes of the semgrep executable (not just 'semgrep scan').

   Some of those exit codes are also (ab)used to represent some error code
   for errors reported in the semgrep CLI JSON output.
*)

(* This ensures that exit codes are declared and documented here. *)
type t = private { code : int; description : string }

(* 'to_int x' is the same as '(x :> int)'. *)
val to_int : t -> int

(* 'of_int' is deprecated. Use the named exit codes below. *)
val of_int : __LOC__:string -> code:int -> description:string -> t

(*
   Standard exit codes.
   All calls to exit must use one of these.

   Example:

     Exit_code.findings ~__LOC__

   __LOC__ is the current source location and it will be logged.
*)
val ok : __LOC__:string -> t
val findings : __LOC__:string -> t
val fatal : __LOC__:string -> t
val invalid_code : __LOC__:string -> t
val invalid_pattern : __LOC__:string -> t
val unparseable_yaml : __LOC__:string -> t
val missing_config : __LOC__:string -> t
val invalid_language : __LOC__:string -> t
val invalid_api_key : __LOC__:string -> t

(* to remove at some point *)
val not_implemented_in_osemgrep : __LOC__:string -> t

val all : t list
(** All the exit codes in use *)

(*
   Test for equality without creating a new exit code that would get logged.
*)
module Equal : sig
  val ok : t -> bool
  val findings : t -> bool
  val fatal : t -> bool
  val invalid_code : t -> bool
  val invalid_pattern : t -> bool
  val unparseable_yaml : t -> bool
  val missing_config : t -> bool
  val invalid_language : t -> bool
  val invalid_api_key : t -> bool
  val not_implemented_in_osemgrep : t -> bool
end

(**
   Alcotest check. This is for tests only.

   Usage:

     Exit_code.Check.findings res.exit_code

   makes Alcotest raise the appropriate exception if res.exit_code is not
   the expected "findings" exit code.

   Use this to avoid the log message occurring when creating a new
   exit code object.

   [quiet]: don't print ASSERT message normally printed by Alcotest
*)
module Check : sig
  val ok : ?quiet:bool -> t -> unit
  val findings : ?quiet:bool -> t -> unit
  val fatal : ?quiet:bool -> t -> unit
  val invalid_code : ?quiet:bool -> t -> unit
  val invalid_pattern : ?quiet:bool -> t -> unit
  val unparseable_yaml : ?quiet:bool -> t -> unit
  val missing_config : ?quiet:bool -> t -> unit
  val invalid_language : ?quiet:bool -> t -> unit
  val invalid_api_key : ?quiet:bool -> t -> unit
  val not_implemented_in_osemgrep : ?quiet:bool -> t -> unit
end
