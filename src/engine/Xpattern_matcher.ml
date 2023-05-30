(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
module MV = Metavariable
module PM = Pattern_match
module RP = Report
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This type and matches_of_matcher() below factorize code between
 * the regexp and spacegrep.
 *)
type ('target_content, 'xpattern) t = {
  (* init returns an option to let the matcher the option to skip
   * certain files (e.g., big binary or minified files for spacegrep)
   *)
  init : filename -> 'target_content option;
  matcher :
    'target_content -> filename -> 'xpattern -> (match_range * MV.bindings) list;
}

(* bugfix: I used to just report one token_location, and if the match
 * was on multiple lines anyway the token_location.str was contain
 * the whole string. However, external programs using a startp/endp
 * expect a different location if the end part is on a different line
 * (e.g., the semgrep Python wrapper), so I now return a pair.
 *)
and match_range = Tok.location * Tok.location

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: same, we should not need that *)
let info_of_token_location loc = Tok.OriginTok loc

let (matches_of_matcher :
      ('xpattern * Xpattern.pattern_id * string) list ->
      ('target_content, 'xpattern) t ->
      filename ->
      RP.times RP.match_result) =
 fun xpatterns matcher file ->
  if xpatterns =*= [] then RP.empty_semgrep_result
  else
    let target_content_opt, parse_time =
      Common.with_time (fun () -> matcher.init file)
    in
    match target_content_opt with
    | None -> RP.empty_semgrep_result (* less: could include parse_time *)
    | Some target_content ->
        let res, match_time =
          Common.with_time (fun () ->
              xpatterns
              |> List.concat_map (fun (xpat, id, pstr) ->
                     let xs = matcher.matcher target_content file xpat in
                     xs
                     |> Common.map (fun ((loc1, loc2), env) ->
                            (* this will be adjusted later *)
                            let rule_id = Match_env.fake_rule_id (id, pstr) in
                            {
                              PM.rule_id;
                              file;
                              range_loc = (loc1, loc2);
                              env;
                              taint_trace = None;
                              tokens = lazy [ info_of_token_location loc1 ];
                              engine_kind = OSS;
                            })))
        in
        RP.make_match_result res Report.ErrorSet.empty
          { RP.parse_time; match_time }

(* todo: same, we should not need that *)
let hmemo = Hashtbl.create 101

let line_col_of_charpos file charpos =
  let conv =
    Common.memoized hmemo file (fun () -> Pos.full_charpos_to_pos_large file)
  in
  conv charpos

(* Like Common2.with_tmp_file but also invalidates the hmemo cache when finished
 *
 * https://github.com/returntocorp/semgrep/issues/5277 *)
let with_tmp_file ~str ~ext f =
  Common2.with_tmp_file ~str ~ext (fun file ->
      Fun.protect
        ~finally:(fun () -> Hashtbl.remove hmemo file)
        (fun () -> f file))

let mval_of_string str t =
  let literal =
    match int_of_string_opt str with
    | Some i -> G.Int (Some i, t)
    (* TODO? could try float_of_string_opt? *)
    | None -> G.String (Tok.unsafe_fake_bracket (str, t))
  in
  MV.E (G.L literal |> G.e)
