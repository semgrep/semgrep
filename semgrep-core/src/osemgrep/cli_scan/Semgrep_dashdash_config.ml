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
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rules_from_dashdash_config config =
  (* TOPORT: resolve rule file URLs; for now we assume it's a file. *)
  Parse_rule.parse_and_filter_invalid_rules config
