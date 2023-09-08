open Common
module E = Error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing of the --config string (e.g., "p/ocaml", "myrules.yaml")

   alt: we could have called this file Semgrep_config.ml, or just Config.ml,
   but it's too vague and could be confused with the other modules
   that can also configure semgrep (the recent .semgrepconfig,
   the Config_semgrep.atd for rule options, and maybe one day even
   a possible .semgrep).

   Partially translated from config_resolver.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* ex: "p/python" *)
type config_string = string [@@deriving show]

(* config_kind is config_string in a parsed form.
 * alt: we could use 't', or just 'config', but 'config' is too vague.
 * 'config_kind' is not much better, because it is vague too, but
 * at least when one sees 'registry_config_kind', it is a bit less
 * ambiguous than 'registry_config' which could be some special settings
 * to interact with the registry.
 *)
type config_kind =
  (* ex: 'foo.yaml' *)
  | File of Fpath.t
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Fpath.t
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  | R of registry_config_kind
  | A of app_config_kind

and registry_config_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
  (* ex: 'pad:basic' *)
  | SavedSnippet of string (* username *) * string (* snippetname *)
  (* pack shortcuts *)
  (* "p/default" *)
  | Auto
  (* "p/r2c" *)
  | R2c

(* Semgrep App shortcuts *)
and app_config_kind = Policy | SupplyChain [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let parse_config_string config_str =
  match config_str with
  | "auto" -> R Auto
  | "r2c" -> R R2c
  | "policy" -> A Policy
  | "supply-chain" -> A SupplyChain
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
  | dir when Sys.file_exists dir && Sys.is_directory dir -> Dir (Fpath.v dir)
  | file when Sys.file_exists file -> File (Fpath.v file)
  (* TOPORT? raise SemgrepError(f"config location `{loc}` is not a file or folder!") *)
  | str ->
      let addendum =
        if !Semgrep_envvars.v.in_docker then
          " (since you are running in docker, you cannot specify arbitrary \
           paths on the host; they must be mounted into the container)"
        else ""
      in
      raise
        (E.Semgrep_error
           ( spf "unable to find a config; path `%s` does not exist%s" str
               addendum,
             Some Exit_code.missing_config ))
