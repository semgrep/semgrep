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

let meth = "semgrep/search"

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The main logic for the /semgrep/search LSP command, which is meant to be
   run on a manual search.
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
    Rule.rules =
  let rules_of_langs (lang_opt : Xlang.t option) : Rule.rules =
    let rules_and_origins =
      Rule_fetching.rules_from_pattern (pattern, lang_opt, fix)
    in
    let rules, _ = Rule_fetching.partition_rules_and_errors rules_and_origins in
    rules
  in
  let xlangs = get_relevant_xlangs env in
  let rules_with_relevant_xlang =
    rules_of_langs None
    |> List.filter (fun (rule : Rule.rule) ->
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
(* Entry point *)
(*****************************************************************************)

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let on_request (server : RPC_server.t) params =
  match Request_params.of_jsonrpc_params params with
  | None -> None
  | Some params ->
      let env = mk_env server params in
      let rules = get_relevant_rules env in
      (* !!calling the engine!! *)
      let matches, _scanned =
        let session =
          {
            server.session with
            user_settings =
              { server.session.user_settings with only_git_dirty = false };
          }
        in
        Scan_helpers.run_semgrep ~targets:env.initial_files
          { server with session } ~rules
      in
      let matches_by_file =
        matches
        |> List_.map (fun (m : OutJ.cli_match) -> (Fpath.to_string m.path, m))
        |> Common2.group_assoc_bykey_eff
      in
      let json =
        List_.map
          (fun (path, matches) ->
            let uri = path |> Uri.of_path |> Uri.to_string in
            let matches =
              matches
              |> List_.map (fun (m : OutJ.cli_match) ->
                     let range_json =
                       Range.yojson_of_t (Conv.range_of_cli_match m)
                     in
                     let fix_json =
                       match m.extra.fix with
                       | None -> `Null
                       | Some s -> `String s
                     in
                     `Assoc [ ("range", range_json); ("fix", fix_json) ])
            in
            `Assoc [ ("uri", `String uri); ("matches", `List matches) ])
          matches_by_file
      in
      Some (`Assoc [ ("locations", `List json) ])
