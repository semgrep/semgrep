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

(* coupling: you must change this if you change the `Request_params.t` type! *)
let mk_params ~lang ~fix pattern =
  let lang =
    match lang with
    | None -> `Null
    | Some lang -> `String (Xlang.to_string lang)
  in
  let fix =
    match fix with
    | None -> `Null
    | Some fix -> `String fix
  in
  let params =
    `Assoc [ ("pattern", `String pattern); ("language", lang); ("fix", fix) ]
  in
  params

module Request_params = struct
  (* coupling: you must change the `mk_params` function above if you change this type! *)
  type t = { pattern : string; lang : Xlang.t option; fix : string option }

  (* This schema means that it matters what order the arguments are in!
     This is a little undesirable, but it's annoying to be truly
     order-agnostic, and this is what `ocaml-lsp` also does.
     https://github.com/ocaml/ocaml-lsp/blob/ad209576feb8127e921358f2e286e68fd60345e7/ocaml-lsp-server/src/custom_requests/req_wrapping_ast_node.ml#L8
  *)
  let of_jsonrpc_params params : t option =
    match params with
    | Some
        (`Assoc
          [
            ("pattern", `String pattern);
            ("language", lang);
            ("fix", fix_pattern);
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
        Some { pattern; lang = lang_opt; fix = fix_opt }
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
(* Information gathering *)
(*****************************************************************************)

let mk_env (server : RPC_server.t) params =
  let scanning_roots =
    List_.map Scanning_root.of_fpath server.session.workspace_folders
  in
  let files =
    server.session.cached_workspace_targets |> Hashtbl.to_seq_values
    |> List.of_seq |> List.concat
  in
  { roots = scanning_roots; initial_files = files; params }

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
let get_relevant_rules ({ params = { pattern; fix; _ }; _ } as env : env) :
    Rule.search_rule list =
  let rules_of_langs (lang_opt : Xlang.t option) : Rule.search_rule list =
    let rules_and_origins =
      Rule_fetching.rules_from_pattern (pattern, lang_opt, fix)
    in
    let rules, _ = Rule_fetching.partition_rules_and_errors rules_and_origins in
    let search_rules, _, _, _ = Rule.partition_rules rules in
    search_rules
  in
  let xlangs = get_relevant_xlangs env in
  let rules_with_relevant_xlang =
    rules_of_langs None
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
      rules_of_langs (Some Xlang.LRegex)
  | other -> other

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

let json_of_matches (matches_by_file : (Fpath.t * Pattern_match.t list) list) =
  let json =
    List_.map
      (fun (path, matches) ->
        let uri = !!path |> Uri.of_path |> Uri.to_string in
        let matches =
          matches
          |> List_.map (fun (m : Pattern_match.t) ->
                 let range_json =
                   Range.yojson_of_t (Conv.range_of_toks m.range_loc)
                 in
                 let fix_json =
                   match m.rule_id.fix with
                   | None -> `Null
                   | Some s -> `String s
                 in
                 `Assoc [ ("range", range_json); ("fix", fix_json) ])
        in
        `Assoc [ ("uri", `String uri); ("matches", `List matches) ])
      matches_by_file
  in
  Some (`Assoc [ ("locations", `List json) ])

(*****************************************************************************)
(* Running Semgrep! *)
(*****************************************************************************)

let next_rules_and_file (server : RPC_server.t) =
  match server.session.search_config with
  | None
  | Some { files = []; _ } ->
      None
  | Some ({ files = file :: rest; _ } as config) ->
      let new_session =
        {
          server.session with
          search_config = Some { config with files = rest };
        }
      in
      Some ((config.rules, file), { server with session = new_session })

let rec search_single_target (server : RPC_server.t) =
  match next_rules_and_file server with
  | None ->
      (* Since we are done with our searches (no more targets), reset our internal state to
         no longer have this scan config.
      *)
      ( json_of_matches [],
        { server with session = { server.session with search_config = None } }
      )
  | Some ((rules, file), server) -> (
      try
        let matches =
          List.concat_map
            (fun rule ->
              (* !!calling the engine!! *)
              match Scan_helpers.run_core_search rule file with
              | None -> []
              | Some matches -> matches)
            rules
        in
        match matches with
        | [] -> search_single_target server
        | _ ->
            let json = json_of_matches [ (file, matches) ] in
            (json, server)
      with
      | Parsing_error.Syntax_error _ -> search_single_target server)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let start_search (server : RPC_server.t) params =
  match Request_params.of_jsonrpc_params params with
  | None ->
      Logs.debug (fun m -> m "no params received in semgrep/search");
      (None, server)
  | Some params ->
      let env = mk_env server params in
      let rules = get_relevant_rules env in
      (* !!calling the engine!! *)
      search_single_target
        {
          server with
          session =
            {
              server.session with
              search_config = Some { rules; files = env.initial_files };
            };
        }

let search_next_file (server : RPC_server.t) _params =
  (* The params are nullary, so we don't actually need to check them. *)
  search_single_target server
