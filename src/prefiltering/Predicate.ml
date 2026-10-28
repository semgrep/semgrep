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
  (* Literal searches are cheaper than arbitrary regexes. *)
  | String _ -> 0
  | Regex _ -> 1

let eval ?utf8_valid (predicate : t) (content : string) : bool =
  let module Search_pattern = Base.String.Search_pattern in
  match predicate with
  | String { needle; case_sensitive } -> (
      let fallback () =
        let pat = Search_pattern.create ~case_sensitive needle in
        Search_pattern.matches pat content
      in
      match Pcre2_.matching_exact_string_bytes ~case_sensitive needle with
      | Error _ -> fallback ()
      | Ok re -> (
          match Pcre2_.is_match re content with
          | Ok matches -> matches
          | Error _ -> fallback ()))
  | Regex re -> (
      match Option.map Lazy_safe.force utf8_valid with
      | Some true ->
          Pcre2_.is_match_valid_utf8 re content |> Result.value ~default:true
      | Some false -> true
      | None -> Pcre2_.unanchored_match re content |> Result.value ~default:true
      )
