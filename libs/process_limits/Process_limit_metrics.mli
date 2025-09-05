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
val record_time_limit :
  info:Exception.timeout_info ->
  result_info:Exception.timeout_result_info ->
  unit
(** [record_time_limit ~info ~result_info] records metrics on if a time limit was
    exceeded, how long we spent, and which function set it *)
