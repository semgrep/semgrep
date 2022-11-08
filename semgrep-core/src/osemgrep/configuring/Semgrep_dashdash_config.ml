open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing of the --config string (e.g., "p/ocaml", "myrules.yaml")

   We could have called this file Semgrep_config.ml, or even just Config.ml,
   but it's too vague and could be confused with the other modules
   that can also configure semgrep (the recent .semgrepconfig,
   the Config_semgrep.atd, for rule options, and maybe one day even
   a possible .semgrep).

   Partially translated from config_resolver.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type config_kind =
  (* ex: 'foo.yaml' *)
  | File of Common.filename
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Common.filename
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  | R of registry_kind

and registry_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* ex: 'pad:basic' *)
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
  | s when s =~ "^http[s]?://" -> URL (Uri.of_string s)
  | s when s =~ "^\\(.*\\):\\(.*\\)" ->
      let user, snippet = Common.matched2 s in
      R (SavedSnippet (user, snippet))
  | dir when Sys.is_directory dir -> Dir dir
  | file when Sys.file_exists file -> File file
  | _else_ -> failwith (spf "not a valid --config string: %s" config_str)

let url_of_registry_kind rkind =
  (* TODO: go through curl interface for now (c/) *)
  let prefix = "https://semgrep.dev/c" in
  let url =
    match rkind with
    | Registry s -> spf "%s/r/%s" prefix s
    | Pack s -> spf "%s/r/%s" prefix s
    | Snippet s -> spf "%s/s/%s" prefix s
    | SavedSnippet (user, snippet) -> spf "%s/%s:%s" prefix user snippet
  in
  Uri.of_string url
