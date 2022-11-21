open Common
module E = Error

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
(* ex: "p/python" *)
type config_str = string [@@deriving show]

(* config_str in a parsed form *)
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
  (* shortcuts *)
  (* "p/default" *)
  | Auto
  (* "p/r2c" *)
  | R2c
  (* Semgrep App shortcuts *)
  | Policy
  | SupplyChain
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let config_kind_of_config_str config_str =
  match config_str with
  | "auto" -> R Auto
  | "r2c" -> R R2c
  | "policy" -> R Policy
  | "supply-chain" -> R SupplyChain
  | s when s =~ "^r/\\(.*\\)" -> R (Registry (Common.matched1 s))
  | s when s =~ "^p/\\(.*\\)" -> R (Pack (Common.matched1 s))
  | s when s =~ "^s/\\(.*\\)" -> R (Snippet (Common.matched1 s))
  (* TODO? could not find a Uri.is_url helper function *)
  | s when s =~ "^http[s]?://" -> URL (Uri.of_string s)
  (* must be after the URL pattern above *)
  | s when s =~ "^\\(.*\\):\\(.*\\)" ->
      let user, snippet = Common.matched2 s in
      R (SavedSnippet (user, snippet))
  (* TOPORT? handle inline rules "rules:..." see python: utils.is_rules() *)
  | dir when Sys.file_exists dir && Sys.is_directory dir -> Dir dir
  | file when Sys.file_exists file -> File file
  (* TOPORT? raise SemgrepError(f"config location `{loc}` is not a file or folder!") *)
  | str ->
      let addendum =
        if Semgrep_envvars.env.in_docker then
          " (since you are running in docker, you cannot specify arbitrary \
           paths on the host; they must be mounted into the container)"
        else ""
      in
      raise
        (E.Semgrep_error
           ( spf "unable to find a config; path `%s` does not exist%s" str
               addendum,
             None ))

(* alt: instead of this Uri.to_string and Uri.of_string, we could
 * use Uri.with_path to adjust the path (a la Filename.concat)
 *)
let url_of_registry_kind rkind =
  (* we go through the CURL interface for now (c/) *)
  let prefix = Uri.to_string Semgrep_envvars.env.semgrep_url ^ "/c" in
  let url =
    match rkind with
    | Registry s -> spf "%s/r/%s" prefix s
    | Pack s -> spf "%s/p/%s" prefix s
    | Snippet s -> spf "%s/s/%s" prefix s
    | SavedSnippet (user, snippet) -> spf "%s/%s:%s" prefix user snippet
    (* LATER: the code below is temporarily comment because handling those
     * shortcuts leads to a 50s slowdown in make osemgrep-e2e; too many tests
     * are relying on those configs which take a long time to download.
     * Those tests should be optimized and use local configs instead.
     *
    | Auto -> spf "%s/p/default" prefix
    | R2c -> spf "%s/p/r2c" prefix
    | Policy -> spf "TODO: handle --config policy"
    | SupplyChain -> spf "TODO: handle --config supply-chain"
     *)
    | _else_ -> failwith (spf "TORESTORE: %s" (show_registry_kind rkind))
  in
  Uri.of_string url
