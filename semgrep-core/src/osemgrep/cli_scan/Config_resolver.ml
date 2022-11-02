open Common

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Partially translated from config_resolver.py

   TODO:
    - handle the registry-aware jsonnet format (LONG)
    - lots of stuff ...
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let load_rules_from_file file =
  logger#debug "loading local config from %s" file;
  Parse_rule.parse_and_filter_invalid_rules file

let rules_from_dashdash_config config_str =
  (* TOPORT: resolve rule file URLs; for now we assume it's a file. *)
  let kind = Semgrep_dashdash_config.config_kind_of_config_str config_str in
  match kind with
  | File file -> load_rules_from_file file
  | R rkind ->
      let url = Semgrep_dashdash_config.url_of_registry_kind rkind in
      logger#debug "trying to download from %s" (Uri.to_string url);
      let content = Network.get url in
      Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
          load_rules_from_file file)
  | _else_ ->
      failwith
        (spf "TODO: config not handled yet: %s"
           (Semgrep_dashdash_config.show_config_kind kind))
