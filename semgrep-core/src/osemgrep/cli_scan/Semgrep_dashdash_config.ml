open Common

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Get the rules from a --config string (e.g., "p/ocaml", "myrules.yaml")

   We could have called this file Semgrep_config.ml, or even just Config.ml,
   but it's too vague and could be confused with the other modules
   that can also configure semgrep (the recent .semgrepconfig,
   the Config_semgrep.atd, for rule options, and maybe one day even
   a possible .semgrep).

   Partially translated from config_resolver.py

   TODO:
    - handle the registry-aware jsonnet format (LONG)
    - lots of stuff ...
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type config_kind =
  | File of Common.filename
  | Dir of string
  | R of registry_kind

and registry_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* pad:basic *)
  | SavedSnippet of string (* username *) * string (* snippetname *)
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let config_kind_of_config_str config_str =
  match config_str with
  | s when s =~ "^r/\\(.*\\)" -> R (Registry (Common.matched1 s))
  | s when s =~ "^p/\\(.*\\)" -> R (Pack (Common.matched1 s))
  | s when s =~ "^s/\\(.*\\)" -> R (Snippet (Common.matched1 s))
  | s when s =~ "^\\(.*\\):\\(.*\\)" ->
      let user, snippet = Common.matched2 s in
      R (SavedSnippet (user, snippet))
  (* TODO  | file when Sys.is_dir file  -> Dir file *)
  | file when Sys.file_exists file -> File file
  | _else_ -> failwith (spf "not a valid --config string: %s" config_str)

let url_of_registry_kind rkind =
  (* TODO: go through curl interface for now (c/) *)
  let prefix = "https://semgrep.dev/c" in
  match rkind with
  | Registry s -> spf "%s/r/%s" prefix s
  | Pack s -> spf "%s/r/%s" prefix s
  | Snippet s -> spf "%s/s/%s" prefix s
  | SavedSnippet (user, snippet) -> spf "%s/%s:%s" prefix user snippet

let load_rules_from_file file =
  logger#debug "loading local config from %s" file;
  Parse_rule.parse_and_filter_invalid_rules file

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rules_from_dashdash_config config_str =
  (* TOPORT: resolve rule file URLs; for now we assume it's a file. *)
  let kind = config_kind_of_config_str config_str in
  match kind with
  | File file -> load_rules_from_file file
  | R rkind ->
      let url = url_of_registry_kind rkind in
      logger#debug "trying to download from %s" url;
      let resp_promise = Quests.get url in
      let resp = Lwt_main.run resp_promise in
      let content = resp.content in
      (* to debug: pr (Quests.Response.show resp); *)
      Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
          load_rules_from_file file)
  | _else_ ->
      failwith (spf "TODO: config not handled yet: %s" (show_config_kind kind))
