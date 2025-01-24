(* Brandon Wu
 *
 * Copyright (C) 2019-2024 Semgrep, Inc.
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
module R = Range
open Lsp
open Lsp.Types
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

let start_meth = "semgrep/search"
let ongoing_meth = "semgrep/searchOngoing"

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The main logic for the /semgrep/search LSP command, which is meant to be
   run on a manual search.

   This is a _streaming_ search, meaning that we service partial results. In
   particular, we choose to service results within the RPC_server loop as
   _one file per request_.

   As such, we have two different commands: /semgrep/search, and /semgrep/searchOngoing.
   Our protocol with them are as follows:
   - when starting a search, the LS receives /semgrep/search and stores some
     information of what to do next in the `server` value
   - the server then receives /semgrep/searchOngoing until either a new search
     is started, or the search is finished.
   - both searches will return the matches found in the next file in the queue

   Effectively, think of `Search.ml` as a pinata, which is consistently hit by
   /semgrep/search and /semgrep/searchOngoing until it runs out of files to
   search.

   TODO: Things that would be nice:
   - AST caching
   - Parallelism (Parmap or threads?)
   - Moving work up to folder-open time rather than search-time
*)

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)

let parse_globs ~kind (strs : Yojson.Safe.t list) =
  strs
  |> List_.filter_map (function
       | `String s -> (
           try
             let loc = Glob.Match.string_loc ~source_kind:(Some kind) s in
             (* We do this because including/excluding does not need to
                specify a precise path.
                For instance, the default behavior of VS Code's search panel
                is if you include something like `a`, then this will
                automatically include any folder named `a`. This includes
                something like `/a`, or even `/b/a`. Essentially, there
                is allowed to be stuff before and after it.
             *)
             let s =
               if Fpath.exists_ext (Fpath.v s) then "**/" ^ s
               else "**/" ^ s ^ "/**"
             in
             Some (Glob.Match.compile ~source:loc (Glob.Parse.parse_string s))
           with
           | Glob.Lexer.Syntax_error _ -> None)
       | _ -> None)

(* coupling: you must change this if you change the `Request_params.t` type! *)
let mk_params ~lang ~fix ~includes ~excludes pattern =
  let lang =
    match lang with
    | None -> `Null
    | Some lang -> `String (Analyzer.to_string lang)
  in
  let fix =
    match fix with
    | None -> `Null
    | Some fix -> `String fix
  in
  let includes = List_.map (fun x -> `String x) includes in
  let excludes = List_.map (fun x -> `String x) excludes in
  let params =
    `Assoc
      [
        ( "patterns",
          `List
            [
              `Assoc [ ("positive", `Bool true); ("pattern", `String pattern) ];
            ] );
        ("language", lang);
        ("fix", fix);
        ("includes", `List includes);
        ("excludes", `List excludes);
      ]
  in
  params

module Request_params = struct
  (* A pattern with an associated positivity to it. *)
  type signed_pattern = {
    (* This `positive` field should be `true` if this pattern is meant to
       be part of a `pattern`, and `false` if it is meant to be part of a
       `pattern-not`.
    *)
    positive : bool;
    pattern : string;
  }

  (* coupling: you must change the `mk_params` function above if you change this type! *)
  type t = {
    patterns : signed_pattern list;
    lang : Analyzer.t option;
    fix : string option;
    includes : Glob.Match.compiled_pattern list;
    excludes : Glob.Match.compiled_pattern list;
  }

  (* This schema means that it matters what order the arguments are in!
     This is a little undesirable, but it's annoying to be truly
     order-agnostic, and this is what `ocaml-lsp` also does.
     https://github.com/ocaml/ocaml-lsp/blob/ad209576feb8127e921358f2e286e68fd60345e7/ocaml-lsp-server/src/custom_requests/req_wrapping_ast_node.ml#L8
  *)
  let of_jsonrpc_params (params : Jsonrpc.Structured.t option) : t option =
    match params with
    | Some
        (`Assoc
          [
            ("patterns", `List patterns);
            ("language", lang);
            ("fix", fix_pattern);
            ("includes", `List includes);
            ("excludes", `List excludes);
          ]) ->
        let patterns =
          List_.filter_map
            (function
              | `Assoc
                  [ ("positive", `Bool positive); ("pattern", `String pattern) ]
                ->
                  Some { positive; pattern }
              | _ -> None)
            patterns
        in
        let lang_opt =
          match lang with
          | `String lang -> Some (Analyzer.of_string lang)
          | _ -> None
        in
        let fix_opt =
          match fix_pattern with
          | `String fix -> Some fix
          | _ -> None
        in
        let includes = parse_globs ~kind:"includes" includes in
        let excludes = parse_globs ~kind:"excludes" excludes in
        Some { patterns; lang = lang_opt; fix = fix_opt; includes; excludes }
    | __else__ -> None

  let _of_jsonrpc_params_exn params : t =
    match of_jsonrpc_params params with
    | None -> failwith "expected jsonrpc schema matching search"
    | Some res -> res
end

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  params : Request_params.t;
  initial_files : Fpath.t list;
  roots : Scanning_root.t list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_by_includes_excludes ~project_root (file : Fpath.t)
    (includes : Glob.Match.compiled_pattern list)
    (excludes : Glob.Match.compiled_pattern list) =
  (* Why must we do this?
     The paths that we receive are absolute paths in the machine.
     This means something like /Users/brandonspark/test/test.py.
     When we match it against a blob, like `test.py`, obviously this
     will not match, because of the giant absolute prefix.
     We actually want the path _relative to the project root_, which is
     `test.py` for the project `test`.
     So we use `Fpath.rem_prefix` here, which emulates that functionality.
  *)
  match Fpath.rem_prefix (Rfpath.to_fpath project_root) file with
  | None ->
      Logs.debug (fun m -> m "file not in project: %s" (Fpath.to_string file));
      false
  | Some file_relative_to_root ->
      let is_included =
        (* if there is not any instance of an `includes`, anything is fair game.
         *)
        match includes with
        | [] -> true
        (* otherwise, you must be mentioned in the includes *)
        | _ ->
            List.exists
              (fun inc -> Glob.Match.run inc !!file_relative_to_root)
              includes
      in
      let is_excluded =
        (* if you have been mentioned in the excludes, you are excluded *)
        List.exists
          (fun exc -> Glob.Match.run exc !!file_relative_to_root)
          excludes
      in
      is_included && not is_excluded

let formula_of_signed_patterns analyzer patterns =
  let of_signed_pattern ({ positive; pattern } : Request_params.signed_pattern)
      =
    let/ xpat = Parse_rule.parse_fake_xpattern analyzer pattern in
    if positive then Ok (Rule.f (Rule.P xpat))
    else Ok (Rule.f (Rule.Not (Tok.unsafe_fake_tok "", Rule.f (Rule.P xpat))))
  in
  match patterns with
  | [ signed_pat ] -> of_signed_pattern signed_pat
  | _ ->
      let/ patterns = List_.map of_signed_pattern patterns |> Base.Result.all in
      Ok (Rule.And (Tok.unsafe_fake_tok "", patterns) |> Rule.f)

(*****************************************************************************)
(* Information gathering *)
(*****************************************************************************)

let filter_out_multiple_python (rules : Rule.search_rule list) :
    Rule.search_rule list =
  let has_python3 =
    List.exists
      (fun x ->
        match x.Rule.target_analyzer with
        | Analyzer.L (Python3, _) -> true
        | _ -> false)
      rules
  in
  (* If a pattern parses with python3, don't run any more Python rules. *)
  if has_python3 then
    List.filter
      (fun x ->
        match x.Rule.target_analyzer with
        | Analyzer.L (Python2, _) -> false
        | Analyzer.L (Python, _) -> false
        | _ -> true)
      rules
  else rules

(* Make an environment for the search. *)

let mk_env (session : Session.t) (params : Request_params.t) =
  let scanning_roots =
    List_.map Scanning_root.of_fpath session.workspace_folders
  in
  let files =
    session.cached_workspace_targets |> Hashtbl.to_seq_values |> List.of_seq
    |> List_.flatten
  in
  let project_root =
    match
      List.nth scanning_roots 0 |> Scanning_root.to_fpath |> Rfpath.of_fpath
    with
    | Error _ -> failwith "somehow unable to get project root from first root"
    | Ok rfpath -> rfpath
  in
  let filtered_by_includes_excludes =
    files
    |> List.filter (fun file ->
           filter_by_includes_excludes ~project_root file params.includes
             params.excludes)
  in
  {
    roots = scanning_roots;
    initial_files = filtered_by_includes_excludes;
    params;
  }

(* Get the languages that are in play in this workspace, by consulting all the
   current targets' languages.
   TODO: Maybe buggy if cached_workspace_targets only has dirty files or
   whatever, I don't think it's always all of the targets in the workspace.
   I tried Find_targets.get_targets, but this ran into an error because of some
   path relativity stuff, I think.
*)
let get_relevant_analyzers (env : env) : Analyzer.t list =
  let lang_set = Hashtbl.create 10 in
  List.iter
    (fun file ->
      let file_langs = Lang.langs_of_filename file in
      List.iter (fun lang -> Hashtbl.replace lang_set lang ()) file_langs)
    env.initial_files;
  Hashtbl.to_seq_keys lang_set |> List.of_seq |> List_.map Analyzer.of_lang

(* Get the rules to run based on the pattern and state of the LSP. *)
let get_relevant_rules ({ params = { patterns; fix; lang; _ }; _ } as env : env)
    : (Rule.search_rule list, Rule_error.t) result =
  (* Get all the possible xpatterns and associated languages for each
     pattern
     This is a map from pattern -> valid langs for that pattern
  *)
  let langs_of_patterns =
    List_.map
      (fun { Request_params.positive = _; pattern } ->
        Rule_fetching.langs_of_pattern (pattern, lang))
      patterns
  in
  (* We want languages which are part of the languages known to the workspace,
     and which also may parse properly in each language.
     Note that we haven't yet enforced that _every_ pattern parses in this
     language, so a valid analyzer might be one which only one pattern successfully
     parses in.
  *)
  let valid_analyzers =
    match langs_of_patterns with
    | [] -> failwith "no patterns given to /semgrep/search"
    | analyzers :: _ ->
        let relevant_analyzers = get_relevant_analyzers env in
        List.filter
          (fun analyzer -> List.mem analyzer relevant_analyzers)
          analyzers
  in
  (* Returns Some if we every pattern is parseable in `analyzer` *)
  let rule_of_lang_opt analyzer : (Rule.search_rule option, Rule_error.t) result
      =
    (* Get all valid lang -> patterns pairings, for valid languages
        which are valid for all patterns
    *)
    let valid_for_all_xpats : bool =
      List.for_all
        (fun analyzers ->
          List.exists
            (fun analyzer' -> Analyzer.equal analyzer analyzer')
            analyzers)
        langs_of_patterns
    in
    if valid_for_all_xpats then
      let/ formula = formula_of_signed_patterns analyzer patterns in
      match Rule.rule_of_formula ?fix analyzer formula with
      | { mode = `Search f; _ } as r ->
          (* repack here so we get the right search_rule type *)
          Ok (Some { r with mode = `Search f })
      | _ -> Ok None
    else Ok None
  in
  let/ search_rules =
    List.fold_left
      (fun acc lang ->
        let/ acc = acc in
        let/ x = rule_of_lang_opt lang in
        Ok
          (match x with
          | Some x -> x :: acc
          | None -> acc))
      (Ok []) valid_analyzers
  in
  match search_rules with
  (* Unfortunately, almost everything parses as YAML, because you can specify
     no quotes and it will be interpreted as a YAML string
     So if we are getting a pattern which only parses as YAML, it's probably
     safe to say it's a non-language-specific pattern.
  *)
  | []
  | [ { target_analyzer = Analyzer.L (Yaml, _); _ } ] ->
      (* should be a singleton *)
      let/ rule = rule_of_lang_opt Analyzer.LRegex in
      Ok (rule |> Option.to_list)
  | other -> Ok (other |> filter_out_multiple_python)

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

(* [first_non_whitespace_after s start] finds the first occurrence
   of a non-whitespace character after index [start] in string [s]*)
let first_non_whitespace_after s start =
  try Str.search_forward (Str.regexp "[^ ]") s start with
  | Not_found -> start

(* TODO: unit tests would be nice *)
let preview_of_line ?(before_length = 12) line ~col_range:(begin_col, end_col) =
  let before_col, is_cut_off =
    let ideal_start = Int.max 0 (begin_col - before_length) in
    let ideal_end = Int.max 0 (begin_col - (before_length * 2)) in
    if Int.equal ideal_start 0 then (first_non_whitespace_after line 0, false)
    else
      (* The picture looks like this:
         xxxxxoooooxxxxxoooooxxxxxoooooxxxxx
                                      ^--^ match
                          ^ ideal_end
              ^ ideal_start
              |___________| ideal range

         the "ideal_start" and "ideal_end" indices denote the ends of the
         "ideal range", which is by default 12-24 characters before the
         start of the match.
         We want to find a "natural beginning" of the preview, such as a space.
         If at all possible, though, it should exist in the ideal range, because
         if our preview starts too early, we won't be able to see the match.
         So we'll look to the left from the ideal end, and hopefully find a good
         place to the right of the ideal start.
         If we don't find a nice starting point, then we'll just go with the ideal start.
      *)
      (* Find the nearest space that occurred before the match
         This is in the hopes of finding a "natural" stopping point.
      *)
      match String.rindex_from_opt line ideal_end ' ' with
      (* We don't want the preview to be too far, though.
         It needs to be at most as early as the ideal start.
         We don't need to call `first_non_whitespace_after` because we know
         this index is right after the closest whitespace to ideal_end.
      *)
      | Some idx when idx > ideal_start -> (idx + 1, false)
      (* This means our preview is currently on whitespace, let's skip ahead if possible.
      *)
      | _ when Char.equal (String.get line ideal_start) ' ' ->
          (first_non_whitespace_after line ideal_start, false)
      (* if we're not on whitespace, we can't do better. Just cut the word in half. *)
      | _ -> (ideal_start, true)
  in
  let before = String.sub line before_col (begin_col - before_col) in
  let inside = String.sub line begin_col (end_col - begin_col) in
  let after = Str.string_after line end_col in
  ((if is_cut_off then "..." ^ before else before), inside, after)

let json_of_matches
    (matches_by_file : (Fpath.t * Core_result.processed_match list) list) =
  let json =
    List_.map
      (fun (path, matches) ->
        let uri = !!path |> Uri.of_path |> Uri.to_string in
        let matches =
          matches
          |> List_.map (fun (m : Core_result.processed_match) ->
                 let range = Conv.range_of_toks m.pm.range_loc in
                 let range_json = Range.yojson_of_t range in
                 let line = List.nth (UFile.cat path) range.start.line in
                 let before, inside, after =
                   if Int.equal range.start.line range.end_.line then
                     preview_of_line line
                       ~col_range:(range.start.character, range.end_.character)
                   else
                     preview_of_line line
                       ~col_range:(range.start.character, String.length line)
                 in
                 let fix_json =
                   match m.autofix_edit with
                   | None -> `Null
                   | Some e -> `String e.replacement_text
                 in
                 `Assoc
                   [
                     ("range", range_json);
                     ("fix", fix_json);
                     ("before", `String before);
                     ("inside", `String inside);
                     ("after", `String after);
                   ])
        in
        `Assoc [ ("uri", `String uri); ("matches", `List matches) ])
      matches_by_file
  in
  `Assoc [ ("locations", `List json) ]

(*****************************************************************************)
(* Running Semgrep! *)
(*****************************************************************************)

let next_rules_and_file (session : Session.t) =
  match session.search_config with
  | None
  | Some { files = []; _ } ->
      None
  | Some ({ files = file :: rest; xconf; _ } as config) ->
      let new_session =
        { session with search_config = Some { config with files = rest } }
      in
      Some (new_session, (config.rules, file, xconf))

let rec search_single_target (session : Session.t) =
  match next_rules_and_file session with
  | None ->
      (* Since we are done with our searches (no more targets), reset our internal state to
         no longer have this scan config.
      *)
      ({ session with search_config = None }, `Assoc [ ("locations", `List []) ])
  | Some (session, (rules, file, xconf)) -> (
      try
        let matches =
          List_.filter_map
            (fun rule ->
              (* !!calling the engine!! *)
              Scan_helpers.run_core_search xconf rule file)
            rules
          |> List_.flatten
        in
        match matches with
        | [] -> search_single_target session
        | _ ->
            let json = json_of_matches [ (file, matches) ] in
            (session, json)
      with
      | Parsing_error.Syntax_error _ -> search_single_target session)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let start_search (session : Session.t) (id : Jsonrpc.Id.t)
    (params : Jsonrpc.Structured.t option) =
  match Request_params.of_jsonrpc_params params with
  | None ->
      Logs.debug (fun m -> m "no params received in semgrep/search");
      (session, Lsp_.Reply.empty)
  | Some params -> (
      let env = mk_env session params in
      match get_relevant_rules env with
      | Error e ->
          Logs.warn (fun m ->
              m "error parsing patterns for semgrep/search: %s"
                (Rule_error.string_of_error e));
          (session, Lsp_.Reply.empty)
      | Ok rules ->
          let xconf =
            {
              Match_env.default_xconfig with
              filter_irrelevant_rules = PrefilterWithCache (Hashtbl.create 10);
            }
          in
          (* !!calling the engine!! *)
          let session, json =
            search_single_target
              {
                session with
                search_config = Some { rules; files = env.initial_files; xconf };
              }
          in
          (session, Lsp_.Reply.now (Lsp_.respond_json id json)))

let search_next_file (session : Session.t) (id : Jsonrpc.Id.t) _params :
    Session.t * Lsp_.Reply.t =
  (* The params are nullary, so we don't actually need to check them. *)
  let session, json = search_single_target session in
  (session, Lsp_.Reply.now (Lsp_.respond_json id json))
