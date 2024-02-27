(* See Pattern_match.ml for more info *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id;
  engine_of_match : Engine_kind.engine_of_finding;
  (* location info *)
  path : Target.path;
  range_loc : Tok.location * Tok.location;
  tokens : Tok.t list Lazy.t;
  env : Metavariable.bindings;
  (* trace *)
  taint_trace : taint_trace Lazy.t option;
  (* for secrets *)
  validation_state : Rule.validation_state;
  severity_override : Rule.severity option;
  metadata_override : JSON.t option;
  dependency : dependency option;
}

and dependency =
  (* Rule had both code patterns and dependency patterns,
     got matches on *both*, the Pattern Match is in code,
     annotated with this dependency match
  *)
  | CodeAndLockfileMatch of dependency_match
  (* Rule had dependency patterns, they matched,
     the Pattern Match is in a lockfile
     So the range_loc of the Dependency.t in this dependency_match
     should be *the same* as the range_loc in the PatternMatch.t
  *)
  | LockfileOnlyMatch of dependency_match

and dependency_match = Dependency.t * Rule.dependency_pattern

(* a record but really only the [id] field should matter *)
and rule_id = {
  id : Rule_ID.t;
  (* extra info useful for Core_json_output *)
  message : string;
  fix : string option;
  fix_regexp : Rule.fix_regexp option;
  langs : Lang.t list;
  pattern_string : string;
}

and taint_trace = taint_trace_item list

and taint_trace_item = {
  source_trace : taint_call_trace;
  tokens : tainted_tokens;
  sink_trace : taint_call_trace;
}

and taint_call_trace =
  | Toks of pattern_match_tokens
  | Call of {
      call_toks : pattern_match_tokens;
      intermediate_vars : tainted_tokens;
      call_trace : taint_call_trace;
    }

and pattern_match_tokens = Tok.t list
and tainted_tokens = Tok.t list [@@deriving show, eq]

(* remove duplicate *)
val uniq : t list -> t list

(* set the engine_kind to `PRO in the match *)
val to_proprietary : t -> t

(* Remove matches that are strictly inside another match *)
val no_submatches : t list -> t list
val range : t -> Range.t
