(* See Pattern_match.ml for more info *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id;
  engine_kind : Engine_kind.t;
  (* location info *)
  file : Fpath.t;
  range_loc : Tok.location * Tok.location;
  tokens : Tok.t list Lazy.t;
  env : Metavariable.bindings;
  (* trace *)
  taint_trace : taint_trace Lazy.t option;
  (* for secrets *)
  validation_state : Rule.validation_state;
  severity_override : Rule.severity option;
  metadata_override : JSON.t option;
  dependency_match : dependency_match option;
}

and dependency_match = AST_generic.dependency * Rule.dependency_pattern

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
