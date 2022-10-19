(*
   External parsers to be registered here by proprietary extensions of semgrep.
*)

(* Exception indicating that a plugin is missing. The argument is
   a human-readable error message. *)
exception Missing_plugin of string

type pattern_parser = string -> AST_generic.any Tree_sitter_run.Parsing_result.t

type target_file_parser =
  Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t

module type T = sig
  (* Register parsing functions for a language. *)
  val register_parsers :
    parse_pattern:pattern_parser -> parse_target:target_file_parser -> unit

  (* Whether the plugin was registered. *)
  val is_available : unit -> bool

  (* Parse a Semgrep pattern or a target.
     Raise the 'Missing_plugin' exception if no parser was registered. *)
  val parse_pattern : pattern_parser
  val parse_target : target_file_parser
end

module Apex : T
