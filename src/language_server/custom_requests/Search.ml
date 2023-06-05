(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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

(* Commentary *)
(* Handles searching workspace by Semgrep pattern *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Yojson.Safe.Util
open Jsonrpc
open Common
open Lsp
open Types
module In = Input_to_core_t
module Out = Output_from_core_t
module XP = Xpattern
module Conv = Convert_utils

let logger = Logging.get_logger [ __MODULE__ ]
(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let meth = "semgrep/search"

(* Taken from osemgrep/networking/Rule_fetching.ml *)
(* Pretty soon we should probably move LSP to OSemgrep *)
let rules_of_pattern pat lang_opt =
  let fk = Tok.unsafe_fake_tok "" in
  let rule_for_xlang xlang =
    let xpat = Parse_rule.parse_xpattern xlang (pat, fk) in
    (* force the parsing of the pattern to get the parse error if any *)
    (match xpat.XP.pat with
    | XP.Sem (lpat, _) -> Lazy.force lpat |> ignore
    | XP.Spacegrep _
    | XP.Aliengrep _
    | XP.Regexp _ ->
        ());
    let rule = Rule.rule_of_xpattern xlang xpat in
    { rule with id = (Rule.ID.of_string "-", fk) }
  in
  match lang_opt with
  | Some lang ->
      (* TODO? capture also parse errors here? and transform the pattern
       * parse error in invalid_rule_error to return in rules_and_origin? *)
      [ rule_for_xlang (Xlang.of_lang lang) ]
  | None ->
      (* We need uniq_by because Lang.assoc contain multiple times the
       * same value, for instance we have ("cpp", Cpp); ("c++", Cpp) in
       * Lang.assoc
       * TODO? use Xlang.assoc instead?
       *)
      let all_langs =
        Lang.assoc
        |> Common.map (fun (_k, l) -> l)
        |> Common.uniq_by ( =*= )
        (* TODO: we currently get a segfault with the Dart parser
         * (for example on a pattern like ': Common.filename'), so we
         * skip Dart for now (which anyway is not really supported).
         *)
        |> Common.exclude (fun x -> x =*= Lang.Dart)
      in
      all_langs
      |> Common.map_filter (fun l ->
             try
               let xlang = Xlang.of_lang l in
               let r = rule_for_xlang xlang in
               Some r
             with
             | Rule.Err _
             | Failure _ ->
                 None)

let search_semgrep config targets pat lang_opt =
  (* We should use Session.targets here, but that uses the LWT monad, so once the git commands use Bos the below can become simpler *)
  let target_mappings = targets.In.target_mappings in
  let rules = rules_of_pattern pat lang_opt in
  let rules =
    Common.mapi
      (fun i r ->
        {
          r with
          Rule.id = (Rule.ID.of_string (string_of_int i), Tok.unsafe_fake_tok "");
        })
      rules
  in
  let rule_ids =
    Common.map (fun r -> fst r.Rule.id |> Rule.ID.to_string) rules
  in
  let rules_by_lang =
    Common.group_by (fun r -> r.Rule.languages.target_analyzer) rules
  in
  let target_mappings =
    Common.map
      (fun (t : In.target) ->
        let rules =
          match List.assoc_opt t.language rules_by_lang with
          | Some rules -> rules
          | None -> []
        in
        let rule_nums =
          Common.map
            (fun r -> fst r.Rule.id |> Rule.ID.to_string |> int_of_string)
            rules
        in
        { t with In.rule_nums })
      target_mappings
  in
  let config =
    {
      config with
      Runner_config.target_source =
        Some (Runner_config.Targets { target_mappings; rule_ids });
    }
  in
  let config = { config with rule_source = Some (Rules rules) } in
  let _, res, _ = Run_semgrep.semgrep_with_raw_results_and_exn_handler config in
  let matches, _ =
    Common.partition_either
      (JSON_report.match_to_match (Some Autofix.render_fix))
      res.matches
  in
  let files_found =
    Common.map (fun (m : Out.core_match) -> m.location) matches
  in
  let found = Common.group_by (fun (m : Out.location) -> m.path) files_found in
  found

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let on_request config targets params =
  match params with
  | None -> None
  | Some params ->
      let params = Structured.yojson_of_t params in
      let pattern = params |> member "pattern" |> to_string in
      let* lang_opt = params |> member "language" |> to_string_option in
      let lang_opt = Lang.of_string_opt lang_opt in
      logger#info "Searching for pattern %s" pattern;
      let locations = search_semgrep config targets pattern lang_opt in
      let json =
        Common.map
          (fun (file, matches) ->
            let uri = file |> Uri.of_path |> Uri.to_string in
            let ranges =
              matches
              |> Common.map Conv.range_of_location
              |> Common.map Range.yojson_of_t
            in
            `Assoc [ ("uri", `String uri); ("ranges", `List ranges) ])
          locations
      in
      Some (`Assoc [ ("locations", `List json) ])
