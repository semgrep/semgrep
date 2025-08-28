(*
   Copyright (c) 2019-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* TODO: like for Manifest_jsonnet, we probably want at some point to return
 * a AST_generic.program instead of a JSON.t
 * TODO: not implemented currently.
 *)
val interpret : Fpath.t -> JSON.t
