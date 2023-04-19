(*
   A level as defined in the gitignore specification.
*)

(*
   A precedence level is a source of compiled gitignore patterns, such as a
   .gitignore file or the command line. Git has rules defining these
   precedence levels.
*)
type t = {
  (* Informational level kind *)
  level_kind : string;
  (* Informational text indicating where the gitignore patterns came from *)
  source_name : string;
  (* Sequence of path selectors derived from gitignore patterns *)
  patterns : Gitignore_syntax.t;
}
