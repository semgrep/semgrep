(* Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details. *)
type t =
  | String of { needle : Base.String.t; case_sensitive : Base.Bool.t }
  | Regex of Pcre2_.t
[@@deriving show, eq, ord, hash, sexp_of]

let eval_cost (predicate : t) : int =
  match predicate with
  (* KMP string search is faster than PCRE2 regex *)
  | String _ -> 0
  | Regex _ -> 1

let eval (predicate : t) (content : string) : bool =
  let module Search_pattern = Base.String.Search_pattern in
  match predicate with
  | String { needle; case_sensitive } ->
      let pat = Search_pattern.create ~case_sensitive needle in
      Search_pattern.matches pat content
  | Regex re -> Pcre2_.unanchored_match ~on_error:true re content
