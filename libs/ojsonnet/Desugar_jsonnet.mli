(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Hook to customize how ojsonnet should resolve import expressions
 * (`local $NAME = import $PATH`). The string args are the base directory
 * of the file currently being processed and the $PATH from the source.
 * The hook returns an AST_jsonnet expression if it can handle the PATH,
 * or None to fall through to the default `local x = import "foo.jsonnet"`
 * file-import behaviour.
 *
 * This callback is useful e.g. in osemgrep to let ojsonnet import yaml
 * files (`local x = import 'foo.yaml'`) or rules from the registry
 * (`local x = import 'p/python'`).
 *
 * Callbacks that do local file IO MUST run candidate paths through
 * [~sandbox] before reading them, otherwise they bypass the import-root
 * check that desugar applies on the default branch (ENGINE-2727).
 *)
type import_callback =
  sandbox:(Fpath.t -> Fpath.t) ->
  string (* base dir *) ->
  string (* import path *) ->
  AST_jsonnet.expr option

val default_callback : import_callback

exception Error of string * Tok.t

(* We pass the original file in addition to its AST so desugar can
 * handle correctly imports by using the dirname of the file as the
 * base directory for imports.
 * The use_std argument is set to true by default and means that
 * the program is first prefixed with 'local std = import "std.jsonnet"
 * where std.jsonnet is the content in Std_jsonnet.std.
 *
 * This function relies on the Conf_ojsonnet.use_std flag.
 *)
val desugar_program :
  ?import_callback:import_callback ->
  Fpath.t ->
  AST_jsonnet.program ->
  Core_jsonnet.program
