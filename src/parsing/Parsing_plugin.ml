(*
   External parsers to be registered here by proprietary extensions of semgrep.
*)

open Common

exception Missing_plugin of string

type pattern_parser = string -> AST_generic.any Tree_sitter_run.Parsing_result.t

type target_file_parser =
  Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t

(* Create and manage the reference holding a plugin. *)
let make lang =
  let parsers = ref None in
  let register ~parse_pattern ~parse_target =
    match !parsers with
    | None -> parsers := Some (parse_pattern, parse_target)
    | Some _existing_parsers ->
        (* This is a bug *)
        let msg =
          spf
            "Plugin initialization error: a %s parser is being registered \
             twice."
            (Lang.to_string lang)
        in
        failwith msg
  in
  let is_available () = !parsers <> None in
  let parse_pattern file =
    match !parsers with
    | None ->
        let msg =
          spf
            "Missing Semgrep extension needed for parsing %s target. Try \
             adding `--pro` to your command."
            (Lang.to_string lang)
        in
        raise (Missing_plugin msg)
    | Some (parse_pattern, _) -> parse_pattern file
  in
  let parse_target file =
    match !parsers with
    | None ->
        let msg =
          spf
            "Missing Semgrep extension needed for parsing %s pattern. Try \
             adding `--pro` to your command."
            (Lang.to_string lang)
        in
        raise (Missing_plugin msg)
    | Some (_, parse_target) -> parse_target file
  in
  (register, is_available, parse_pattern, parse_target)

module type T = sig
  val register_parsers :
    parse_pattern:pattern_parser -> parse_target:target_file_parser -> unit

  val is_available : unit -> bool
  val parse_pattern : pattern_parser
  val parse_target : target_file_parser
end

module Apex = struct
  let register_parsers, is_available, parse_pattern, parse_target =
    make Lang.Apex
end
