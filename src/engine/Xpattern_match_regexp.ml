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

(* This is generic so that internal uses of regex can still use PCRE (instead
   of PCRE2) to get the old semantics. Notably, this is used by generic mode.
*)
type ('regex, 'libregex, 'substring) re_functions = {
  get_pattern : 'regex -> string;
  get_regex : 'regex -> 'libregex;
  exec_all_noerr : 'regex -> string -> 'substring array;
  get_substring : 'substring -> int -> string;
  get_substring_ofs : 'substring -> int -> int * int;
  names : 'libregex -> string array;
  num_of_subs : 'substring -> int;
  get_named_substring_ofs : 'libregex -> string -> 'substring -> int * int;
  get_named_substring : 'libregex -> string -> 'substring -> string;
}

let pcre2_regex_functions =
  {
    get_pattern = Pcre2_.pcre_pattern;
    get_regex = Pcre2_.pcre_regexp;
    exec_all_noerr = (fun rex s -> Pcre2_.exec_all_noerr ~rex s);
    get_substring = Pcre2.get_substring;
    get_substring_ofs = Pcre2.get_substring_ofs;
    names = Pcre2.names;
    num_of_subs = Pcre2.num_of_subs;
    get_named_substring_ofs = Pcre2.get_named_substring_ofs;
    get_named_substring = Pcre2.get_named_substring;
  }

let pcre_regex_functions =
  {
    get_pattern = (fun ({ pattern; _ } : Pcre_.t) -> pattern);
    get_regex = (fun ({ regexp; _ } : Pcre_.t) -> regexp);
    exec_all_noerr = (fun rex s -> Pcre_.exec_all_noerr ~rex s);
    get_substring = Pcre.get_substring;
    get_substring_ofs = Pcre.get_substring_ofs;
    names = Pcre.names;
    num_of_subs = Pcre.num_of_subs;
    get_named_substring_ofs = Pcre.get_named_substring_ofs;
    get_named_substring = Pcre.get_named_substring;
  }
[@@alert "-deprecated"]

let regexp_matcher ?(base_offset = 0) regex_functions big_str (file : Fpath.t)
    (regexp : 'regex) =
  let subs = regex_functions.exec_all_noerr regexp big_str in
  subs |> Array.to_list
  |> List_.map (fun sub ->
         (* Below, we add `base_offset` to any instance of `bytepos`, because
            the `bytepos` we obtain is only within the range of the string
            being searched, which may itself be offset from a larger file.

            By maintaining this base offset, we can accurately recreate the
            original line/col, at minimum cost.
         *)
         let matched_str = regex_functions.get_substring sub 0 in
         let bytepos, _ = regex_functions.get_substring_ofs sub 0 in
         let bytepos = bytepos + base_offset in
         let str = matched_str in
         let line, column = line_col_of_charpos file bytepos in
         let pos = Pos.make file ~line ~column bytepos in
         let loc1 = { Tok.str; pos } in

         let bytepos = bytepos + String.length str in
         let str = "" in
         let line, column = line_col_of_charpos file bytepos in
         let pos = Pos.make file ~line ~column bytepos in
         let loc2 = { Tok.str; pos } in

         (* the names of all capture groups within the regexp *)
         let names =
           regex_functions.names (regex_functions.get_regex regexp)
           |> Array.to_list
         in
         (* return regexp bound group $1 $2 etc *)
         let n = regex_functions.num_of_subs sub in
         (* TODO: remove when we kill numeric capture groups *)
         let numbers_env =
           match n with
           | 1 -> []
           | _ when n <= 0 -> raise Impossible
           | n ->
               List_.enum 1 (n - 1)
               |> List_.filter_map (fun n ->
                      try
                        let bytepos, _ =
                          regex_functions.get_substring_ofs sub n
                        in
                        let str = regex_functions.get_substring sub n in
                        let line, column = line_col_of_charpos file bytepos in
                        let pos = Pos.make file ~line ~column bytepos in
                        let loc = { Tok.str; pos } in
                        let t = Tok.tok_of_loc loc in
                        Some (spf "$%d" n, MV.Text (str, t, t))
                      with
                      | Not_found ->
                          Log.debug (fun m ->
                              m "not found %d substring of %s in %s" n
                                (regex_functions.get_pattern regexp)
                                matched_str);
                          None)
         in
         let names_env =
           names
           |> List_.filter_map (fun name ->
                  try
                    (* TODO: make exception-free versions of the missing
                       functions in SPcre. *)
                    let bytepos, _ =
                      regex_functions.get_named_substring_ofs
                        (regex_functions.get_regex regexp)
                        name sub
                    in
                    let bytepos = bytepos + base_offset in
                    let str =
                      regex_functions.get_named_substring
                        (regex_functions.get_regex regexp)
                        name sub
                    in
                    let line, column = line_col_of_charpos file bytepos in
                    let pos = Pos.make file ~line ~column bytepos in
                    let loc = { Tok.str; pos } in
                    let t = Tok.tok_of_loc loc in
                    Some (spf "$%s" name, MV.Text (str, t, t))
                  with
                  | Not_found ->
                      Log.debug (fun m ->
                          m "not found %s substring of %s in %s" name
                            (regex_functions.get_pattern regexp)
                            matched_str);
                      None)
         in
         ((loc1, loc2), names_env @ numbers_env))

let matches_of_regexs regexps lazy_content (file : Fpath.t) origin =
  matches_of_matcher regexps
    {
      init = (fun _ -> Some (Lazy.force lazy_content));
      matcher = regexp_matcher pcre2_regex_functions;
    }
    file origin
[@@profiling]
