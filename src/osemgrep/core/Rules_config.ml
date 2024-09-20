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

(* t is config_string in a parsed form *)
type t =
  (* ex: 'foo.yaml' *)
  | File of Fpath.t
  (* ex: 'myrules/' (will go also recursively in subdirs of myrules) *)
  | Dir of Fpath.t
  (* ex: 'https://raw.githubusercontent.com/r2c/semgrep-rules/template.yaml' *)
  | URL of Uri.t
  | R of registry_config_kind
  | A of app_config_kind

(* Not a great name but at least when one sees 'registry_config_kind',
 * it is a bit less ambiguous than 'registry_config' which could be some
 * special settings to interact with the registry.
 *)
and registry_config_kind =
  (* r/... *)
  | Registry of string
  (* p/... *)
  | Pack of string
  (* s/... *)
  | Snippet of string
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
let parse_config_string ~in_docker (config_str : config_string) : t =
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
  (* TOPORT? handle inline rules "rules:..." see python: utils.is_rules() *)
  | dir when Sys.file_exists dir && Sys.is_directory dir -> Dir (Fpath.v dir)
  (* TODO: this does not work when we are run from jsonnet rules which
   * can include other files in which case we should adjust the pwd.
   * For example `make check` currently prints
   * [00.28][WARNING]: bad config string: forbid_exit.jsonnet
   *)
  | file when Sys.file_exists file -> File (Fpath.v file)
  (* TOPORT? raise SemgrepError(f"config location `{loc}` is not a file or folder!") *)
  | str ->
      (* TODO: Logs.warn (fun m -> m "bad config string: %s" str);
       * restore once the TODO above is fixed
       *)
      let addendum =
        (* old: was !Semgrep_envvars.v.in_docker but that introduce
         * a dependency cycle between configuring/ and core/
         *)
        if in_docker then
          " (since you are running in docker, you cannot specify arbitrary \
           paths on the host; they must be mounted into the container)"
        else ""
      in
      raise
        (E.Semgrep_error
           ( spf "unable to find a config; path `%s` does not exist%s" str
               addendum,
             Some (Exit_code.missing_config ~__LOC__) ))
