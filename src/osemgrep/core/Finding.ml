(*
   A semgrep finding.

   Translated from rule_match.py
*)

(*
   All the types that used to be returned by semgrep-core as JSON.
   The source is /semgrep-core/src/core/semgrep_output_v1.atd
*)
module C = Output_from_core_t

(* hopefully we don't really need this *)
type json = Yojson.Safe.t

(*
   A unique key designed with data-completeness & correctness in mind.

   Results in more unique findings than ci_unique_key.

   Used for deduplication in the CLI before writing output.
*)
type cli_unique_key = {
  rule_id : string;
  path : string;
  start_offset : int;
  end_offset : int;
  message : string;
}

(*
   A unique key designed with notification user experience in mind.

   Results in fewer unique findings than cli_unique_key.
*)
type ci_unique_key = {
  rule_id : string;
  path : string;
  syntactic_context : string;
  index : int;
}

(*
   Used to sort findings in output.

   Note that we often batch by rule ID when gathering matches,
   so the included self.rule_id will not do anything in those cases.

   The message field is included to ensure a consistent ordering
   when two findings match with different metavariables on the same code.
*)
type ordering_key = {
  path : string;
  start : C.position;
  end_ : C.position;
  rule_id : string;
  message : string;
}

(*
   A unique key with match based id's notion of uniqueness in mind.

   We use this to check if two different findings will have the same match
   based id or not. This is so we can then index them accordingly so two
   similar findings will have unique match based IDs
*)
type match_based_key = {
  match_formula_str : string;
  path : string;
  rule_id : string;
}

(*
    A section of code that matches a single rule (which is potentially many
    patterns).

    This is also often referred to as a finding.
    TODO: Rename this class to Finding?
*)
type _t = {
  match_ : C.core_match;
  (* fields from the rule *)
  message : string;
  severity : Severity.rule_severity;
  metadata : string * json list (* ??? Dict[str, Any] *);
  (*
     Do not use this extra field! This prevents from having typed JSON output
     TODO: instead of extra, we should use the more explicit fields:
      fixed_lines: Optional[Any] = field(default=None)
      dependency_match_only: Optional[bool] = field(default=None)
      dependency_matches: Optional[Any] = field(default=None)
     but then this would require to remove the @frozen from this class
     because autofix and dependency_aware and join_rule are actually monkey patching
     this frozen class.
     TODO: redundant with core.extra but we do some monkey patching on
     this extra field which prevents to use directly core.extra (immutable)
  *)
  extra : json;
  (*
     fields derived from the rule
     We call rstrip() for consistency with semgrep-core, which ignores
     whitespace including newline chars at the end of multiline patterns.
  *)
  fix : string option; (* field(converter=rstrip, default=None) *)
  fix_regex : C.fix_regex option;
  (* ??? *)
  index : int; (* defaults to 0 *)
  (* Used only for indexing match based IDs since index uses syntactic IDs to
     index meaning that there can be index collisions if we use it for mid *)
  match_based_index : int; (* defaults to 0 *)
  (* This is the accompanying formula from the rule that created the match
     Used for pattern_based_id

     This could be derived, if we wanted to keep the rule as a field of the
     match. Seems easier to just calculate it w/index *)
  match_formula_string : string; (* default to "" *)
  (* None means we didn't check; ignore status is unknown *)
  is_ignored : bool option;
  (* derived attributes *)
  lines : string list;
  previous_line : string;
  syntactic_context : string;
  cli_unique_key : cli_unique_key;
  ci_unique_key : ci_unique_key;
  ordering_key : ordering_key;
  match_based_key : match_based_key;
  syntactic_id : string;
  match_based_id : string;
}
