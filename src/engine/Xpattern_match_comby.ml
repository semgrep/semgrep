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
module CK = Comby_kernel
module MS = CK.Matchers.Metasyntax

let logger = Logging.get_logger [ __MODULE__ ]

(* less: "if you need line/column conversion [in Comby], then there's another
 * function to run over matches to hydrate line/column info in the result"
 * src: https://github.com/comby-tools/comby/issues/244
 *)
let line_col_charpos_of_comby_range file range =
  let { CK.Match.Location.offset = charpos; line = _; column = _ } =
    range.CK.Match.Range.match_start
  in
  (* reusing line_col_of_charpos is fine for now *)
  let line, col = line_col_of_charpos file charpos in
  (line, col, charpos)

let comby_matcher (m_all, source) file pat =
  let matches = m_all ~template:pat ~source () in
  (*Format.printf "%a@." CK.Match.pp_json_lines (None, matches);*)
  matches
  |> Common.map (fun { CK.Match.range; environment; matched } ->
         let env =
           CK.Match.Environment.vars environment
           |> Common.map (fun s ->
                  let mvar = "$" ^ s in
                  let str_opt = CK.Match.Environment.lookup environment s in
                  let range_opt =
                    CK.Match.Environment.lookup_range environment s
                  in
                  match (str_opt, range_opt) with
                  | Some str, Some range ->
                      let line, column, charpos =
                        line_col_charpos_of_comby_range file range
                      in
                      let loc = { PI.str; charpos; file; line; column } in
                      let t = info_of_token_location loc in
                      let mval = mval_of_string str t in
                      (mvar, mval)
                  | __else__ -> raise Impossible)
         in
         let line, column, charpos =
           line_col_charpos_of_comby_range file range
         in
         let loc1 = { PI.str = matched; charpos; file; line; column } in

         let charpos2 = charpos + String.length matched in
         let line, column = line_col_of_charpos file charpos2 in
         let loc2 = { PI.str = ""; charpos = charpos2; file; line; column } in

         ((loc1, loc2), env))

let matches_of_combys combys lazy_content file =
  matches_of_matcher combys
    {
      init =
        (fun _ ->
          let _d, _b, e = Common2.dbe_of_filename file in
          let metasyntax =
            {
              MS.syntax =
                [ MS.Hole (Everything, MS.Delimited (Some "$", None)) ];
              identifier = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            }
          in
          match
            CK.Matchers.Alpha.select_with_extension ~metasyntax ("." ^ e)
          with
          | None ->
              logger#info "no Alpha Comby module for extension %s" e;
              None
          | Some x ->
              let (module M : CK.Matchers.Matcher.S) = x in
              Some
                ( (fun ~template ~source () -> M.all ~template ~source ()),
                  Lazy.force lazy_content ));
      matcher = comby_matcher;
    }
    file
  [@@profiling]
