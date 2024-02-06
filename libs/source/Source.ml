(* Cooper Pierce
 *
 * Copyright (c) Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* See Source.mli for top-level documentation of this module. *)

type t = File of Fpath.t [@@deriving show, eq, ord]

(* Module for the purpose of implementing sexp conversions for t. We cannot
 * derive sexp since Fpath does not have an sexp implementation, so we convert
 * to a string and then derive sexp on the updated type.
 *)
module S = struct
  open Sexplib.Std

  type t = File of string [@@deriving sexp]
end

let sexp_of_t source =
  let x =
    match source with
    | File path -> S.File (Fpath.to_string path)
  in
  S.sexp_of_t x

let t_of_sexp sexp =
  match S.t_of_sexp sexp with
  | S.File path -> File (Fpath.v path)

let to_string (s : t) =
  match s with
  | File path -> Fpath.to_string path

let to_string_opt ?(unspecified = "unknown") (s : t option) =
  match s with
  | Some s -> to_string s
  | None -> Printf.sprintf "<%s>" unspecified
