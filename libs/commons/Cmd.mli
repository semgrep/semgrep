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
(* Build "commands" to be executed by UCmd.ml *)

(* command name, e.g. "git" *)
type name = Name of string [@@deriving show]

(* command arguments *)
type args = string list [@@deriving show]

(* the whole command *)
type t = name * args [@@deriving show]

(* Cmd is a small wrapper around Bos.Cmd so we rely on Bos for
 * most operations
 *)
val bos_apply : (Bos.Cmd.t -> 'a) -> t -> 'a

type run_status = Bos.OS.Cmd.run_status

(* environment variables *)
type env = { vars : (string * string) list; inherit_parent_env : bool }

(* [env_of_list vars] will generate an environment to pass to the
 * UCmd.xxx_of_run functions to execute an external program with
 * environment variables set as described in [vars]. Note that
 * by default inherit_parent_env is set to true meaning the environment
 * variables specified in [vars] are added to the environment of the
 * parent process (and possibly overriding them).
 *)
val env_of_list : ?inherit_parent_env:bool -> (string * string) list -> env

(* for error messages *)
val to_string : t -> string
