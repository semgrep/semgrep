(* See Core_match.ml for more info *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id;
  engine_of_match : Engine_kind.engine_of_finding;
  env : Metavariable.bindings;
  (* location info *)
  path : Target.path;
  range_loc : Tok.location * Tok.location;
  ast_node : AST_generic.any option;
  tokens : Tok.t list Lazy.t;
  (* trace *)
  taint_trace : Taint_trace.t Lazy.t option;
  (* for SCA *)
  sca_match : SCA_match.t option;
  (* for Secrets *)
  validation_state : Rule.validation_state;
  severity_override : Rule.severity option;
  metadata_override : JSON.t option;
  (* A field to be populated based on intra-formula `fix` keys.
     This is _prior_ to AST-based autofix and interpolation, which occurs in
     Autofix.ml.
  *)
  fix_text : string option;
  (* facts (known truths derived from the cfg) of a match.
     it is used for filtering out matches that do not satisfy the
     comparison condition. this field is added here so that
     it can be passed into and used in Match_search_mode.filter_ranges.
  *)
  facts : AST_generic.facts;
}

(* a record but really only the [id] field should matter *)
and rule_id = {
  id : Rule_ID.t;
  (* extra info useful for Core_json_output *)
  message : string;
  metadata : JSON.t option;
  fix : string option;
  fix_regexp : Rule.fix_regexp option;
  langs : Lang.t list;
  pattern_string : string;
}
[@@deriving show, eq]

(* remove duplicate *)
val uniq : t list -> t list

(* set the engine_kind to `PRO in the match *)
val to_proprietary : t -> t

(* Remove matches that are strictly inside another match *)
val no_submatches : t list -> t list
val range : t -> Range.t
