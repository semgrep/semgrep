(*
   External parsers to be registered here by proprietary extensions of semgrep.
*)

(* Exception indicating that a plugin is missing. The argument is
   a human-readable error message. *)
exception Missing_plugin of string

type pattern_parser = string -> AST_generic.any Tree_sitter_run.Parsing_result.t

type target_file_parser =
  Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t

(* Return an error message in case of a missing plugin. *)
val check_if_missing : Lang.t -> (unit, string) Result.t

(* Call 'check_is_missing' if any target programming language with a missing
   plugin is involved with this analyzer. *)
val check_if_missing_analyzer : Xlang.t -> (unit, string) Result.t

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
