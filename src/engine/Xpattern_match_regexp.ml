(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 Semgrep Inc.
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
open Common
open Xpattern_matcher
module MV = Metavariable
module Log = Log_engine.Log

let regexp_matcher ?(base_offset = 0) big_str (file : Fpath.t)
    (regexp : Pcre2_.t) =
  (* the names of all capture groups within the regexp; a property of the
     regexp itself, so computed once rather than per match *)
  let names = Pcre2_.capture_groups regexp in
  let match_result_of_captures sub =
    (* Below, we add `base_offset` to any instance of `bytepos`, because
            the `bytepos` we obtain is only within the range of the string
            being searched, which may itself be offset from a larger file.

            By maintaining this base offset, we can accurately recreate the
            original line/col, at minimum cost.
         *)
    let whole_match =
      match Pcre2_.match_of_captures sub 0 with
      | Some m -> m
      | None ->
          (* group 0 is the whole match, which always participates *)
          assert false
    in
    let matched_str = Pcre2_.substring_of_match whole_match in
    let Pcre2_.{ start = bytepos; _ } = Pcre2_.range_of_match whole_match in
    let bytepos = bytepos + base_offset in
    let str = matched_str in
    let line, column = line_col_of_charpos file bytepos in
    let pos = Pos.make file ~line ~column bytepos in
    let loc1 = { Loc.str; pos } in

    let bytepos = bytepos + String.length str in
    let str = "" in
    let line, column = line_col_of_charpos file bytepos in
    let pos = Pos.make file ~line ~column bytepos in
    let loc2 = { Loc.str; pos } in

    (* [Some (mvar, binding)] if the group matched by [m_opt] participated
         in the match, shifting its position by [offset]; [None] (with a debug
         log naming the group via [group_desc]) otherwise. *)
    let binding_of_group ~offset mvar group_desc m_opt =
      match m_opt with
      | Some m ->
          let str = Pcre2_.substring_of_match m in
          let Pcre2_.{ start = bytepos; _ } = Pcre2_.range_of_match m in
          let bytepos = bytepos + offset in
          let line, column = line_col_of_charpos file bytepos in
          let pos = Pos.make file ~line ~column bytepos in
          let loc = { Loc.str; pos } in
          let t = Tok.tok_of_loc loc in
          Some (mvar, MV.Text (str, t, t))
      | None ->
          Log.debug (fun m ->
              m "not found %s substring of %s in %s" group_desc
                regexp.Pcre2_.pattern matched_str);
          None
    in
    (* return regexp bound group $1 $2 etc *)
    let n = Pcre2_.captures_length sub in
    (* TODO: remove when we kill numeric capture groups *)
    let numbers_env =
      match n with
      | 1 -> []
      | _ when n <= 0 -> raise Impossible
      | n ->
          (* NB: historically, [base_offset] has not been applied to the
               positions of numeric groups, only named ones. *)
          List_.enum 1 (n - 1)
          |> List.filter_map (fun i ->
              Pcre2_.match_of_captures sub i
              |> binding_of_group ~offset:0 (spf "$%d" i) (string_of_int i))
    in
    let names_env =
      names
      |> List.filter_map (fun (name, group_number) ->
          Pcre2_.match_of_captures sub group_number
          |> binding_of_group ~offset:base_offset (spf "$%s" name) name)
    in
    ((loc1, loc2), names_env @ numbers_env)
  in
  let rec collect_matches acc captures =
    match captures () with
    | Seq.Nil -> Ok (List.rev acc)
    | Seq.Cons (Ok sub, rest) ->
        collect_matches (match_result_of_captures sub :: acc) rest
    | Seq.Cons (Error err, _) -> Error err
  in
  match Pcre2_.captures_iter regexp big_str |> collect_matches [] with
  | Ok matches -> matches
  | Error err ->
      Log.warn (fun m ->
          m "PCRE2 error while matching pattern-regex: %a" Pcre2.pp_match_error
            err);
      (* Match errors invalidate the whole result, including any matches
         successfully processed before the error. *)
      []

let matches_of_regexs regexps lazy_content (file : Fpath.t) origin =
  matches_of_matcher regexps
    {
      init =
        (fun _ ->
          let content, time = Common.force_lazy_with_time lazy_content in
          (Some content, time));
      matcher = regexp_matcher;
    }
    file origin
[@@profiling]
