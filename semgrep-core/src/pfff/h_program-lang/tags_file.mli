type tag = {
  tag_definition_text : string;
  tagname : string;
  line_number : int;
  (* offset of beginning of tag_definition_text, when have 0-indexed filepos *)
  byte_offset : int;
  (* only used by vim *)
  kind : Entity_code.entity_kind;
}

(* will generate a TAGS file in the current directory *)
val generate_TAGS_file :
  Common.filename -> (Common.filename * tag list) list -> unit

(* will generate a tags file in the current directory *)
val generate_vi_tags_file :
  Common.filename -> (Common.filename * tag list) list -> unit

val add_method_tags_when_unambiguous :
  (Common.filename * tag list) list -> (Common.filename * tag list) list

(* internals *)
val mk_tag : string -> string -> int -> int -> Entity_code.entity_kind -> tag
val string_of_tag : tag -> string
val header : string
val footer : string

(* helpers used by language taggers *)
val tag_of_info : string array -> Parse_info.t -> Entity_code.entity_kind -> tag
