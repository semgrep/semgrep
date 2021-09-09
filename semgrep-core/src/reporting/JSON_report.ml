(*s: semgrep/reporting/JSON_report.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
open Common
module StrSet = Common2.StringSet
open AST_generic
module V = Visitor_AST
module PI = Parse_info
module E = Semgrep_error_code
module J = JSON
module MV = Metavariable
module RP = Report
open Pattern_match
module ST = Semgrep_core_response_t (* atdgen definitions *)

module SJ = Semgrep_core_response_j (* JSON conversions *)

(*****************************************************************************)
(* Unique ID *)
(*****************************************************************************)
(*s: function [[JSON_report.string_of_resolved]] *)
(*e: function [[JSON_report.string_of_resolved]] *)

(*s: function [[JSON_report.unique_id]] *)
(* Returning scoping-aware information about a metavariable, so that
 * the callers of semgrep (semgrep python) can check if multiple metavariables
 * reference the same entity, or reference exactly the same code.
 * See Naming_AST.ml for more information.
 *)
let unique_id any =
  match any with
  | E { e = N (Id (_, { id_resolved = { contents = Some (_, sid) }; _ })); _ }
    ->
      { ST.type_ = `ID; md5sum = None; sid = Some sid }
  (* not an Id, return a md5sum of its AST as a "single unique id" *)
  | _ ->
      (* todo? note that if the any use a parameter, or a local,
       * as in foo(x): return complex(x), then they will have different
       * md5sum because the parameter will be different! We may
       * want to abstract also the resolved information in those cases.
       *)
      let any = AST_generic_helpers.abstract_for_comparison_any any in
      (* alt: Using the AST dumper should work also.
       * let v = Meta_AST.vof_any any in
       * let s = OCaml.string_of_v v in
       *)
      let s = Marshal.to_string any [] in
      let md5 = Digest.string s in
      { ST.type_ = `AST; md5sum = Some (Digest.to_hex md5); sid = None }

(*e: function [[JSON_report.unique_id]] *)

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

(*s: function [[JSON_report.json_range]] *)
let position_range min_loc max_loc =
  (* pfff (and Emacs) have the first column at index 0, but not r2c *)
  let adjust_column x = x + 1 in

  let len_max = String.length max_loc.PI.str in
  ( {
      ST.line = min_loc.PI.line;
      col = adjust_column min_loc.PI.column;
      offset = min_loc.PI.charpos;
    },
    {
      ST.line = max_loc.PI.line;
      col = adjust_column (max_loc.PI.column + len_max);
      offset = max_loc.PI.charpos + len_max;
    } )

(*e: function [[JSON_report.json_range]] *)

(*s: function [[JSON_report.range_of_any]] *)
let range_of_any_opt startp_of_match_range any =
  let empty_range = (startp_of_match_range, startp_of_match_range) in
  match any with
  (* those are ok and we don't want to generate a NoTokenLocation for those.
   * alt: change Semgrep.atd to make optional startp/endp for metavar_value.
   *)
  | Ss [] | Args [] -> Some empty_range
  | _ ->
      let ( let* ) = Common.( >>= ) in
      let* min_loc, max_loc = V.range_of_any_opt any in
      let startp, endp = position_range min_loc max_loc in
      Some (startp, endp)

(*e: function [[JSON_report.range_of_any]] *)

(*s: function [[JSON_report.json_metavar]] *)
let metavars startp_of_match_range (s, mval) =
  let any = MV.mvalue_to_any mval in
  match range_of_any_opt startp_of_match_range any with
  | None ->
      raise
        (Parse_info.NoTokenLocation
           (spf "NoTokenLocation with metavar %s, close location = %s" s
              (SJ.string_of_position startp_of_match_range)))
  | Some (startp, endp) ->
      ( s,
        {
          ST.start = startp;
          end_ = endp;
          abstract_content =
            any |> V.ii_of_any
            |> List.filter PI.is_origintok
            |> List.sort Parse_info.compare_pos
            |> List.map PI.str_of_info
            |> Matching_report.join_with_space_if_needed;
          unique_id = unique_id any;
        } )

(*e: function [[JSON_report.json_metavar]] *)

(*s: function [[JSON_report.match_to_json]] *)
let match_to_match x =
  try
    let min_loc, max_loc = x.range_loc in
    let startp, endp = position_range min_loc max_loc in
    Left
      ({
         ST.rule_id = Some x.rule_id.id;
         location =
           {
             path = x.file;
             start = startp;
             end_ = endp;
             lines = [] (* ?? spacegrep? *);
           };
         extra =
           {
             message = Some x.rule_id.message;
             metavars = x.env |> List.map (metavars startp);
           };
       }
        : ST.match_)
    (* raised by min_max_ii_by_pos in range_of_any when the AST of the
     * pattern in x.code or the metavar does not contain any token
     *)
  with Parse_info.NoTokenLocation s ->
    let loc = Parse_info.first_loc_of_file x.file in
    let s =
      spf "NoTokenLocation with pattern %s, %s" x.rule_id.pattern_string s
    in
    let err = E.mk_error_loc loc (E.MatchingError s) in
    Right err
  [@@profiling]

(*e: function [[JSON_report.match_to_json]] *)

(* was in pfff/h_program-lang/R2c.ml becore *)
let hcache = Hashtbl.create 101

let lines_of_file (file : Common.filename) : string array =
  Common.memoized hcache file (fun () ->
      try Common.cat file |> Array.of_list with _ -> [| "EMPTY FILE" |])

let error_to_error err =
  let file = err.E.loc.PI.file in
  let lines = lines_of_file file in
  let startp, endp = position_range err.E.loc err.E.loc in
  let line = err.E.loc.PI.line in
  let rule_id = E.check_id_of_error_kind err.E.typ in
  let error_type = E.string_of_error_kind err.E.typ in
  {
    error_type;
    ST.rule_id = Some rule_id;
    location =
      Some
        {
          path = file;
          start = startp;
          end_ = endp;
          lines = (try [ lines.(line - 1) ] with _ -> [ "NO LINE" ]);
        };
    message = "EMMA_TODO";
    details = None;
    yaml_path = None;
  }

let json_time_of_profiling_data profiling_data =
  {
    ST.targets =
      profiling_data.RP.file_times
      |> List.map (fun { RP.file = target; parse_time; match_time; run_time } ->
             { ST.path = target; parse_time; match_time; run_time });
    rule_parse_time = Some profiling_data.RP.rule_parse_time;
  }

let match_results_of_matches_and_errors files res =
  let matches, new_errs =
    Common.partition_either match_to_match res.RP.matches
  in
  let errs = !E.g_errors @ new_errs @ res.RP.errors in
  let files_with_errors =
    List.fold_left
      (fun acc err -> StrSet.add err.E.loc.file acc)
      StrSet.empty errs
  in
  let count_errors = StrSet.cardinal files_with_errors in
  let count_ok = List.length files - count_errors in
  {
    ST.matches;
    errors = errs |> List.map error_to_error;
    skipped = res.RP.skipped;
    stats = { okfiles = count_ok; errorfiles = count_errors };
    time = res.RP.rule_profiling |> Common.map_opt json_time_of_profiling_data;
  }
  [@@profiling]

let json_of_profile_info profile_start =
  let now = Unix.gettimeofday () in
  (* total time, but excluding J.string_of_json time that comes after *)
  (* partial copy paste of Common.adjust_profile_entry *)
  Hashtbl.add !Common._profile_table "TOTAL" (ref (now -. profile_start), ref 1);

  (* partial copy paste of Common.profile_diagnostic *)
  let xs =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) !Common._profile_table []
    |> List.sort (fun (_k1, (t1, _n1)) (_k2, (t2, _n2)) -> compare t2 t1)
  in
  xs
  |> List.map (fun (k, (t, cnt)) ->
         (k, J.Object [ ("time", J.Float !t); ("count", J.Int !cnt) ]))
  |> fun xs -> J.Object xs

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* This function is used for non-target related exns (e.g., parsing a rule).
 * It is called in Main.ml as a last resort instead of returning
 * matching results (which can also contain errors).

 * The Parse_info.Parsing_error and other exns in Parse_info, which are
 * raised during a target analysis (parsing, naming, matching, etc.), are
 * captured in Main.ml by Semgrep_error_code.exn_to_error. The function below is
 * for all the other non-target related exns.
 * update: we actually now use the generic AST to parse a YAML rule, so
 * we may raise Parse_info.Other_Error in a non-target context too.
 *
 * invariant: every non-target related exn that has a Parse_info.t should
 * be captured here!
 * TODO:
 *  - use the _posTODO below
 *  - handle Yaml_to_generic.Parse_error
 *  - handle more exn in Parse_rule.ml? covered everything?
 *  - handle Parse_info.Other_error and more, which can now be raised
 *    when parsing a rule.
 * todo? move all non-pfff exns to a central file Error_semgrep.ml?
 *
 * coupling: Test.metachecker_regression_tests
 *)
let _json_of_exn e =
  match e with
  | Rule.InvalidRule (rule_id, msg, _posTODO) ->
      J.Object
        [
          ("rule_id", J.String rule_id);
          ("error", J.String "invalid rule");
          ("message", J.String msg);
        ]
  | Rule.InvalidLanguage (rule_id, language, _posTODO) ->
      J.Object
        [
          ("rule_id", J.String rule_id);
          ("error", J.String "invalid language");
          ("language", J.String language);
        ]
  | Rule.InvalidPattern (rule_id, pattern, xlang, message, pos, path) ->
      let lang = Rule.string_of_xlang xlang in
      let range_json =
        match pos with
        | { token = PI.FakeTokStr (str, _); _ } -> J.String str
        | _ ->
            let s_loc = PI.token_location_of_info pos in
            J.Object
              [
                ("file", J.String s_loc.file);
                ("line", J.Int s_loc.line);
                ("col", J.Int s_loc.column);
                ( "path",
                  J.Array
                    (List.map
                       (fun x ->
                         match int_of_string_opt x with
                         | Some i -> J.Int i
                         | None -> J.String x)
                       (List.rev path)) );
              ]
      in
      J.Object
        [
          ("rule_id", J.String rule_id);
          ("error", J.String "invalid pattern");
          ("pattern", J.String pattern);
          ("language", J.String lang);
          ("message", J.String message);
          ("range", range_json);
        ]
  | Rule.InvalidRegexp (rule_id, message, _posTODO) ->
      J.Object
        [
          ("rule_id", J.String rule_id);
          ("error", J.String "invalid regexp in rule");
          ("message", J.String message);
        ]
  | Rule.InvalidYaml (msg, _posTODO) ->
      J.Object [ ("error", J.String "invalid yaml"); ("message", J.String msg) ]
  | Rule.UnparsableYamlException msg ->
      J.Object
        [ ("error", J.String "unparsable yaml"); ("message", J.String msg) ]
  (* Other exns (Failure, Timeout, etc.) without position information :(
   * Not much we can do.
   *)
  | exn ->
      J.Object
        [
          ("error", J.String "unknown exception");
          ("message", J.String (Common.exn_to_s exn));
        ]

(*s: function [[JSON_report.error]] *)
(* this is used only in the testing code, to reuse the
 * Semgrep_error_code.compare_actual_to_expected
 *)
let error loc (rule : Pattern_match.rule_id) =
  E.error_loc loc (E.SemgrepMatchFound (rule.id, rule.message))

(*e: function [[JSON_report.error]] *)

(*s: function [[JSON_report.match_to_error]] *)
let match_to_error x =
  let min_loc, _max_loc = x.range_loc in
  error min_loc x.rule_id

(*e: function [[JSON_report.match_to_error]] *)
(*e: semgrep/reporting/JSON_report.ml *)
