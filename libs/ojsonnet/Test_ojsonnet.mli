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
val dump_jsonnet_ast : Fpath.t -> unit
val dump_jsonnet_core : Fpath.t -> unit
val dump_jsonnet_value : Fpath.t -> unit
val dump_jsonnet_json : Fpath.t -> unit
val perf_test_jsonnet : Fpath.t -> unit
