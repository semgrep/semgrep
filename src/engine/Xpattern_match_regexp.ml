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
  let subs = Pcre2_.exec_all_noerr ~rex:regexp big_str in
  let lib_regex = Pcre2_.pcre_regexp regexp in
  subs |> Array.to_list
  |> List.map (fun sub ->
      (* Below, we add `base_offset` to any instance of `bytepos`, because
            the `bytepos` we obtain is only within the range of the string
            being searched, which may itself be offset from a larger file.

            By maintaining this base offset, we can accurately recreate the
            original line/col, at minimum cost.
         *)
      let matched_str = Pcre2.get_substring sub 0 in
      let bytepos, _ = Pcre2.get_substring_ofs sub 0 in
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

      (* the names of all capture groups within the regexp *)
      let names = Pcre2.names lib_regex |> Array.to_list in
      (* return regexp bound group $1 $2 etc *)
      let n = Pcre2.num_of_subs sub in
      (* TODO: remove when we kill numeric capture groups *)
      let numbers_env =
        match n with
        | 1 -> []
        | _ when n <= 0 -> raise Impossible
        | n ->
            List_.enum 1 (n - 1)
            |> List.filter_map (fun n ->
                try
                  let bytepos, _ = Pcre2.get_substring_ofs sub n in
                  let str = Pcre2.get_substring sub n in
                  let line, column = line_col_of_charpos file bytepos in
                  let pos = Pos.make file ~line ~column bytepos in
                  let loc = { Loc.str; pos } in
                  let t = Tok.tok_of_loc loc in
                  Some (spf "$%d" n, MV.Text (str, t, t))
                with
                | Not_found ->
                    Log.debug (fun m ->
                        m "not found %d substring of %s in %s" n
                          (Pcre2_.pcre_pattern regexp)
                          matched_str);
                    None)
      in
      let names_env =
        names
        |> List.filter_map (fun name ->
            try
              (* TODO: make exception-free versions of the missing
                       functions in Pcre2_. *)
              let bytepos, _ =
                Pcre2.get_named_substring_ofs lib_regex name sub
              in
              let bytepos = bytepos + base_offset in
              let str = Pcre2.get_named_substring lib_regex name sub in
              let line, column = line_col_of_charpos file bytepos in
              let pos = Pos.make file ~line ~column bytepos in
              let loc = { Loc.str; pos } in
              let t = Tok.tok_of_loc loc in
              Some (spf "$%s" name, MV.Text (str, t, t))
            with
            | Not_found ->
                Log.debug (fun m ->
                    m "not found %s substring of %s in %s" name
                      (Pcre2_.pcre_pattern regexp)
                      matched_str);
                None)
      in
      ((loc1, loc2), names_env @ numbers_env))

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
