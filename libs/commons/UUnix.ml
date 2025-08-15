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
