(*
   A level as defined in the gitignore specification.
*)

type t = {
  level_kind : string;
  source_name : string;
  patterns : Gitignore_syntax.path_selector list;
}
