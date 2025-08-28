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
val parse : Fpath.t -> AST_yaml.document
val any : ?src_path:Fpath.t -> string -> AST_yaml.any

(* internals used by yaml_to_generic.ml *)
val parse_yaml_file : is_target:bool -> Fpath.t -> string -> AST_yaml.document
