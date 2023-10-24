(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 r2c
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

let logger = Logging.get_logger [ __MODULE__ ]

let regexp_matcher ?(base_offset = 0) big_str file regexp =
  let subs = SPcre.exec_all_noerr ~rex:regexp big_str in
  subs |> Array.to_list
  |> Common.map (fun sub ->
         (* Below, we add `base_offset` to any instance of `bytepos`, because
            the `bytepos` we obtain is only within the range of the string
            being searched, which may itself be offset from a larger file.

            By maintaining this base offset, we can accurately recreate the
            original line/col, at minimum cost.
         *)
         let matched_str = Pcre.get_substring sub 0 in
         let bytepos, _ = Pcre.get_substring_ofs sub 0 in
         let bytepos = bytepos + base_offset in
         let str = matched_str in
         let line, column = line_col_of_charpos file bytepos in
         let pos = Pos.make ~file ~line ~column bytepos in
         let loc1 = { Tok.str; pos } in

         let bytepos = bytepos + String.length str in
         let str = "" in
         let line, column = line_col_of_charpos file bytepos in
         let pos = Pos.make ~file ~line ~column bytepos in
         let loc2 = { Tok.str; pos } in

         (* the names of all capture groups within the regexp *)
         let names = Pcre.names regexp.regexp |> Array.to_list in
         (* return regexp bound group $1 $2 etc *)
         let n = Pcre.num_of_subs sub in
         (* TODO: remove when we kill numeric capture groups *)
         let numbers_env =
           match n with
           | 1 -> []
           | _ when n <= 0 -> raise Impossible
           | n ->
               Common2.enum 1 (n - 1)
               |> Common.map_filter (fun n ->
                      try
                        let bytepos, _ = Pcre.get_substring_ofs sub n in
                        let str = Pcre.get_substring sub n in
                        let line, column = line_col_of_charpos file bytepos in
                        let pos = Pos.make ~file ~line ~column bytepos in
                        let loc = { Tok.str; pos } in
                        let t = Tok.tok_of_loc loc in
                        Some (spf "$%d" n, MV.Text (str, t, t))
                      with
                      | Not_found ->
                          logger#debug "not found %d substring of %s in %s" n
                            regexp.pattern matched_str;
                          None)
         in
         let names_env =
           names
           |> Common.map_filter (fun name ->
                  try
                    (* TODO: make exception-free versions of the missing
                       functions in SPcre. *)
                    let bytepos, _ =
                      Pcre.get_named_substring_ofs regexp.regexp name sub
                    in
                    let bytepos = bytepos + base_offset in
                    let str = Pcre.get_named_substring regexp.regexp name sub in
                    let line, column = line_col_of_charpos file bytepos in
                    let pos = Pos.make ~file ~line ~column bytepos in
                    let loc = { Tok.str; pos } in
                    let t = Tok.tok_of_loc loc in
                    Some (spf "$%s" name, MV.Text (str, t, t))
                  with
                  | Not_found ->
                      logger#debug "not found %s substring of %s in %s" name
                        regexp.pattern matched_str;
                      None)
         in
         ((loc1, loc2), names_env @ numbers_env))

let matches_of_regexs regexps lazy_content file =
  matches_of_matcher regexps
    {
      init = (fun _ -> Some (Lazy.force lazy_content));
      matcher = regexp_matcher;
    }
    file
[@@profiling]
