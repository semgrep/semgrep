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
open Fpath_.Operators

let stat x =
  (* nosemgrep: dont-use-unix-stat *)
  try Ok (Unix.stat !!x) with
  | Unix.Unix_error (code, func, arg) -> Error (code, func, arg)

let lstat x =
  (* nosemgrep: dont-use-unix-stat *)
  try Ok (Unix.lstat !!x) with
  | Unix.Unix_error (code, func, arg) -> Error (code, func, arg)

let fstat x =
  (* nosemgrep: dont-use-unix-stat *)
  try Ok (Unix.fstat x) with
  | Unix.Unix_error (code, func, arg) -> Error (code, func, arg)
