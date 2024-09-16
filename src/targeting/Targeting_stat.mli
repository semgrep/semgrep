type stat = {
  kind : Unix.file_kind;
  line_count : int;
  minified : bool;
  size : int;
  textual : bool;
  type_ : File_type.file_type;
}
[@@deriving yojson]
(** [stat] is like Linux's `stat`, but more specific to Semgrep *)

type annotated_target = {
  internal_path : string;
  targets : Target.t list;
  stat : stat;
}
[@@deriving yojson]
(** [annotated_target] is a list of targets, their shared path, and the [stat]
    of that file*)

type annotated_target_list = annotated_target list [@@deriving yojson]
(** [annotated_target_list] is just a list of [annotated_target]. This type
    mostly exists to make Yojson operations easier *)

val annotate_targets : Target.t list -> annotated_target_list
(** [annotate_targets targets] will group targets by their shared
    internal_paths, then attach extra info turning them into [annotated_target] *)
