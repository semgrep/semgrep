(*
   External parsers (referenced here as "plugins") registered in semgrep-pro.
*)

type pattern_parser =
  string (* pattern content *) ->
  (AST_generic.any, unit) Tree_sitter_run.Parsing_result.t

type target_file_parser =
  Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t

module type T = sig
  (* If true, the plugin provides only an alternate implementation to
     one that is available by default. *)
  val is_optional : bool

  (* Register parsing functions for a language (called in semgrep-pro) *)
  val register_parsers :
    parse_pattern:pattern_parser -> parse_target:target_file_parser -> unit

  (* Whether the plugin was registered. *)
  val is_available : unit -> bool

  (* Parse a Semgrep pattern or a target.
     Raise the 'Missing_plugin' exception if no parser was registered. *)
  val parse_pattern : pattern_parser
  val parse_target : target_file_parser
end

(* Plugins for the current external parsers. Those plugins are
 * referenced in parsing_languages/Parse_target2.ml and Parse_pattern2.ml
 * and raise Missing_plugin when the plugin is not registered.
 *)
module Apex : T
module Csharp : T
module Elixir : T

(* Exception indicating that a plugin is missing. The argument is
   a human-readable error message. *)
exception Missing_plugin of string

(* Return an error message in case of a missing plugin that is not optional.
   This function is called in Parse_rule.ml to filter rules using languages
   that can not be handled because their corresponding plugins was not
   registered.
*)
val check_if_missing : Lang.t -> (unit, string) Result.t

(* Call 'check_is_missing' if any target programming language with a missing
   plugin is involved with this analyzer. *)
val check_if_missing_analyzer : Analyzer.t -> (unit, string) Result.t
val all_possible_plugins : Lang.t list
