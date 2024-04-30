(* See Pattern_match.ml for more info *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id;
  (* location info *)
  path : Target.path;
  range_loc : Tok.location * Tok.location;
  tokens : Tok.t list Lazy.t;
  env : Metavariable.bindings;
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
val submatch : t -> t -> bool

(* Remove matches that are strictly inside another match *)
val no_submatches : t list -> t list
val range : t -> Range.t
