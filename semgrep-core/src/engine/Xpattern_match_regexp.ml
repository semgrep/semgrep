(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Xpattern_matcher

let logger = Logging.get_logger [ __MODULE__ ]

let regexp_matcher big_str file regexp =
  let re_src = Regexp_engine.pcre_pattern regexp in
  let re = Regexp_engine.pcre_regexp regexp in
  let subs = SPcre.exec_all_noerr ~rex:re big_str in
  subs |> Array.to_list
  |> Common.map (fun sub ->
         let matched_str = Pcre.get_substring sub 0 in
         let charpos, _ = Pcre.get_substring_ofs sub 0 in
         let str = matched_str in
         let line, column = line_col_of_charpos file charpos in
         let loc1 = { PI.str; charpos; file; line; column } in

         let charpos = charpos + String.length str in
         let str = "" in
         let line, column = line_col_of_charpos file charpos in
         let loc2 = { PI.str; charpos; file; line; column } in

         (* return regexp bound group $1 $2 etc *)
         let n = Pcre.num_of_subs sub in
         let env =
           match n with
           | 1 -> []
           | _ when n <= 0 -> raise Impossible
           | n ->
               Common2.enum 1 (n - 1)
               |> Common.map_filter (fun n ->
                      try
                        let charpos, _ = Pcre.get_substring_ofs sub n in
                        let str = Pcre.get_substring sub n in
                        let line, column = line_col_of_charpos file charpos in
                        let loc = { PI.str; charpos; file; line; column } in
                        let t = PI.mk_info_of_loc loc in
                        Some (spf "$%d" n, MV.Text (str, t))
                      with
                      | Not_found ->
                          logger#debug "not found %d substring of %s in %s" n
                            re_src matched_str;
                          None)
         in
         ((loc1, loc2), env))

let matches_of_regexs regexps lazy_content file =
  matches_of_matcher regexps
    {
      init = (fun _ -> Some (Lazy.force lazy_content));
      matcher = regexp_matcher;
    }
    file
  [@@profiling]
