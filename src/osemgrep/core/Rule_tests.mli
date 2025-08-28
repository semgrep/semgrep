(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val is_config_fixtest_suffix : Fpath.t -> bool
val is_config_test_suffix : Fpath.t -> bool
val is_config_suffix : Fpath.t -> bool
val get_config_filenames : Fpath.t -> Fpath.t list

val get_config_test_filenames :
  original_config:Fpath.t ->
  configs:Fpath.t list ->
  original_target:Fpath.t ->
  (Fpath.t * Fpath.t list) list
