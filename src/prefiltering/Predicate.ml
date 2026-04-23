(* Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details. *)
open Sexplib.Std

(* Needed to derive hash *)
let hash_fold_string : Base.Hash.state -> string -> Base.Hash.state =
  Base.hash_fold_string

type t = String of string | Regex of Pcre2_.t
[@@deriving show, eq, ord, hash, sexp_of]

let eval (predicate : t) (content : string) : bool =
  match predicate with
  | String needle ->
      let module Search_pattern = Base.String.Search_pattern in
      let pat = Search_pattern.create needle in
      Search_pattern.matches pat content
  | Regex re -> Pcre2_.unanchored_match ~on_error:true re content
