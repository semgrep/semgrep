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

module R = Range
open Lsp
open Lsp.Types
open Fpath_.Operators
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

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
  |> List_.map_filter (function
       | `String s -> (
           try
             let loc = Glob.Match.string_loc ~source_kind:(Some kind) s in
             Some (Glob.Match.compile ~source:loc (Glob.Parse.parse_string s))
           with
           | Glob.Lexer.Syntax_error _ -> None)
       | _ -> None)

module Request_params = struct
  type t = {
    pattern : string;
    lang : Xlang.t option;
    fix : string option;
    includes : Glob.Match.compiled_pattern list;
    excludes : Glob.Match.compiled_pattern list;
  }

  let of_jsonrpc_params (params : Jsonrpc.Structured.t option) : t option =
    match params with
    | Some
        (`Assoc
          [
            ("pattern", `String pattern);
            ("language", lang);
            ("fix", fix_pattern);
            ("includes", `List includes);
            ("excludes", `List excludes);
          ]) ->
        let lang_opt =
          match lang with
          | `String lang -> Some (Xlang.of_string lang)
          | _ -> None
        in
        let fix_opt =
          match fix_pattern with
          | `String fix -> Some fix
          | _ -> None
        in
        let includes = parse_globs ~kind:"includes" includes in
        let excludes = parse_globs ~kind:"excludes" excludes in
        Some { pattern; lang = lang_opt; fix = fix_opt; includes; excludes }
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

let parse_and_resolve_name (lang : Lang.t) (fpath : Fpath.t) :
    AST_generic.program * Tok.location list =
  let { Parsing_result2.ast; skipped_tokens; _ } =
    Parse_target.parse_and_resolve_name lang fpath
  in
  (ast, skipped_tokens)

let filter_by_gitignore ~project_root (files : Fpath.t list) : Fpath.t list =
  (* Filter all files by gitignores *)
  let gitignore_filter =
    Gitignore_filter.create ~project_root:(Rfpath.to_fpath project_root) ()
  in
  files
  |> List.filter (fun file ->
         match Ppath.in_project ~root:project_root file with
         | Error _ -> failwith "err"
         | Ok ppath -> (
             match Gitignore_filter.select gitignore_filter [] ppath with
             | Gitignore.Ignored, _ -> false
             | _ -> true))

(*****************************************************************************)
(* Information gathering *)
(*****************************************************************************)

let mk_env (server : RPC_server.t) (params : Request_params.t) =
  let scanning_roots =
    List_.map Scanning_root.of_fpath server.session.workspace_folders
  in
  let files =
    server.session.cached_workspace_targets |> Hashtbl.to_seq_values
    |> List.of_seq |> List.concat
  in
  let project_root =
    match
      List.nth scanning_roots 0 |> Scanning_root.to_fpath |> Rfpath.of_fpath
    with
    | Error _ -> failwith "somehow unable to get project root from first root"
    | Ok rfpath -> rfpath
  in
  (* Filter all files by gitignores *)
  let filtered_files =
    (* Gitignore code. We hard-code it to be false for now.
       This adds 10s to startup time in `semgrep-app` on my machine.
       Gitignore is expensive! I don't know why.
    *)
    if false then filter_by_gitignore ~project_root files else files
  in
  (* TODO: This has a bug!!!
     Suppose we exclude `test.py` and are given `tests2/test.py`.
     This code will not exclude properly, on the basis that `test.py`
     does not match `tests2/test.py`.
     Essentially, we may need to look at every suffix of the file to see
     if it matches.
  *)
  let filtered_by_includes_excludes =
    filtered_files
    |> List.filter (fun file ->
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
               Logs.debug (fun m ->
                   m "file not in project: %s" (Fpath.to_string file));
               false
           | Some file_relative_to_root ->
               let is_not_included =
                 match params.includes with
                 | [] -> false
                 (* if there wasn't a single included which included you, you are excluded *)
                 | _ ->
                     not
                       (List.exists
                          (fun inc ->
                            Glob.Match.run inc !!file_relative_to_root)
                          params.includes)
               in
               let is_not_excluded =
                 match params.excludes with
                 | [] -> true
                 (* if there wasn't a single excluded which excluded you, you are included *)
                 | _ ->
                     not
                       (List.exists
                          (fun exc ->
                            Glob.Match.run exc !!file_relative_to_root)
                          params.excludes)
               in
               (not is_not_included) && is_not_excluded)
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
let get_relevant_xlangs (env : env) : Xlang.t list =
  let lang_set = Hashtbl.create 10 in
  List.iter
    (fun file ->
      let file_langs = Lang.langs_of_filename file in
      List.iter (fun lang -> Hashtbl.replace lang_set lang ()) file_langs)
    env.initial_files;
  Hashtbl.to_seq_keys lang_set |> List.of_seq |> List_.map Xlang.of_lang

(* Get the rules to run based on the pattern and state of the LSP. *)
let get_relevant_rules ({ params = { pattern; fix; lang; _ }; _ } as env : env)
    : Rule.search_rule list =
  (* TODO: figure out why rules_from_rules_source_async hangs *)
  (* let src = Rules_source.(Pattern (pattern, xlang_opt, None)) in *)
  let search_rules_of_langs (lang_opt : Xlang.t option) : Rule.search_rule list
      =
    let rules_and_origins =
      Rule_fetching.rules_from_pattern (pattern, lang_opt, fix)
    in
    let rules, _ = Rule_fetching.partition_rules_and_errors rules_and_origins in
    let search_rules, _, _, _ = Rule.partition_rules rules in
    search_rules
  in
  let xlangs = get_relevant_xlangs env in
  let rules_with_relevant_xlang =
    search_rules_of_langs lang
    |> List.filter (fun (rule : Rule.search_rule) ->
           List.mem rule.target_analyzer xlangs)
  in
  match rules_with_relevant_xlang with
  (* Unfortunately, almost everything parses as YAML, because you can specify
     no quotes and it will be interpreted as a YAML string
     So if we are getting a pattern which only parses as YAML, it's probably
     safe to say it's a non-language-specific pattern.
  *)
  | []
  | [ { target_analyzer = Xlang.L (Yaml, _); _ } ] ->
      (* should be a singleton *)
      search_rules_of_langs (Some Xlang.LRegex)
  | other ->
      let has_python3 =
        List.exists
          (fun x ->
            match x.Rule.target_analyzer with
            | Xlang.L (Python3, _) -> true
            | _ -> false)
          other
      in
      (* If a pattern parses with python3, don't run any more Python rules. *)
      if has_python3 then
        List.filter
          (fun x ->
            match x.Rule.target_analyzer with
            | Xlang.L (Python2, _) -> false
            | Xlang.L (Python, _) -> false
            | _ -> true)
          other
      else other

let get_rules_and_targets (env : env) =
  let rules = get_relevant_rules env in
  (* Ideally we would just use this, but it seems to err.
     I suspect that Find_targets.git_list_files is not quite correct.
  *)
  (* let _res = Find_targets.get_target_fpaths Scan_CLI.default.targeting_conf env.roots in *)
  let rules_and_targets =
    rules
    |> List_.map (fun (rule : Rule.search_rule) ->
           let xlang = rule.target_analyzer in
           ( rule,
             env.initial_files
             |> List_.map (fun file ->
                    Xtarget.resolve parse_and_resolve_name
                      (Target.mk_regular xlang Product.all (File file))) ))
  in
  rules_and_targets

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
    if ideal_start = 0 then (first_non_whitespace_after line 0, false)
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
      | _ when String.get line ideal_start = ' ' ->
          (first_non_whitespace_after line ideal_start, false)
      (* if we're not on whitespace, we can't do better. Just cut the word in half. *)
      | _ -> (ideal_start, true)
  in
  let before = String.sub line before_col (begin_col - before_col) in
  let inside = String.sub line begin_col (end_col - begin_col) in
  let after = Str.string_after line end_col in
  ((if is_cut_off then "..." ^ before else before), inside, after)

let json_of_matches (xtarget : Xtarget.t)
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
                 let line =
                   List.nth
                     (Common2.lines (Lazy.force xtarget.lazy_content))
                     range.start.line
                 in
                 let before, inside, after =
                   if range.start.line = range.end_.line then
                     preview_of_line line
                       ~col_range:(range.start.character, range.end_.character)
                   else
                     preview_of_line line
                       ~col_range:(range.start.character, String.length line)
                 in
                 let fix_json =
                   match m.autofix_edit with
                   | None -> `Null
                   | Some edit -> `String edit.replacement_text
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
  Some (`Assoc [ ("locations", `List json) ])

(*****************************************************************************)
(* Running Semgrep! *)
(*****************************************************************************)

let rec next_rule_and_xtarget (server : RPC_server.t) =
  match server.session.search_config with
  | None
  | Some { rules_and_targets = []; _ } ->
      None
  | Some ({ rules_and_targets = (_rule, []) :: rest; _ } as conf) ->
      let new_session =
        {
          server.session with
          search_config = Some { conf with rules_and_targets = rest };
        }
      in
      next_rule_and_xtarget { server with session = new_session }
  | Some
      ({ rules_and_targets = (rule, xtarget :: rest_xtargets) :: rest; _ } as
       conf) ->
      let new_session =
        {
          server.session with
          search_config =
            Some { conf with rules_and_targets = (rule, rest_xtargets) :: rest };
        }
      in
      Some ((rule, xtarget, conf), { server with session = new_session })

let rec search_single_target (server : RPC_server.t) =
  let hook _file _pm = () in
  match next_rule_and_xtarget server with
  | None ->
      (* Since we are done with our searches (no more targets), reset our internal state to
         no longer have this scan config.
      *)
      ( Some (`Assoc [ ("locations", `List []) ]),
        { server with session = { server.session with search_config = None } }
      )
  | Some ((rule, xtarget, conf), server) -> (
      match xtarget.path.origin with
      (* This shouldn't happen. *)
      | GitBlob _ -> search_single_target server
      | File path ->
          let is_relevant_rule =
            Match_rules.is_relevant_rule_for_xtarget
              (rule :> Rule.rule)
              conf.xconf xtarget
          in
          let is_relevant_lang =
            (* We have to look at all the initial files again when we do this.
                TODO: Maybe could be better to infer languages from each file,
                so we only have to look at each file once.
            *)
            Filter_target.filter_target_for_xlang rule.target_analyzer path
          in
          if is_relevant_rule && is_relevant_lang then
            try
              (* !!calling the engine!! *)
              let ({ Core_result.matches; _ } : _ Core_result.match_result) =
                Match_search_mode.check_rule rule hook conf.xconf xtarget
              in
              let matches_with_fixes =
                matches
                |> List_.map Core_result.mk_processed_match
                |> Autofix.produce_autofixes
              in
              match matches with
              | [] -> search_single_target server
              | _ ->
                  let json =
                    json_of_matches xtarget [ (path, matches_with_fixes) ]
                  in
                  (json, server)
            with
            | Parsing_error.Syntax_error _ -> search_single_target server
          else search_single_target server)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let start_search (server : RPC_server.t) (params : Jsonrpc.Structured.t option)
    =
  match Request_params.of_jsonrpc_params params with
  | None ->
      Logs.debug (fun m -> m "no params received in semgrep/search");
      (None, server)
  | Some params ->
      let env = mk_env server params in
      let rules_and_targets = get_rules_and_targets env in
      let xconf =
        {
          Match_env.default_xconfig with
          filter_irrelevant_rules = PrefilterWithCache (Hashtbl.create 10);
        }
      in
      (* !!calling the engine!! *)
      search_single_target
        {
          server with
          session =
            {
              server.session with
              search_config = Some { rules_and_targets; xconf };
            };
        }

let search_next_file (server : RPC_server.t) _params =
  (* The params are nullary, so we don't actually need to check them. *)
  search_single_target server
