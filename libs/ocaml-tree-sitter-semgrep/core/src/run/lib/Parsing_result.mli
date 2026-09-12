(*
   A semi-generic type used to hold the results of parsing a file.
*)

type stat = {
  total_line_count: int;
  error_line_count: int; (* number of lines affected by one or more errors *)
  error_count: int;
}

(*
   There's one type for the program which is the grammar's entrypoint,
   and one type for all the extras which are independent entrypoints.
   For example:

     type program = ...
     type extra =
     | Comment of ...
     | Heredoc of ...
*)
type ('program, 'extra) t = {
  program: 'program option;
  extras: 'extra list;
  errors: Tree_sitter_error.t list;
  stat: stat;
}

val create :
  Src_file.t -> 'a option -> 'b list -> Tree_sitter_error.t list -> ('a, 'b) t

val export_stat : out_file:string -> stat -> unit
