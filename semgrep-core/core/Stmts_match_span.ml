(*
   Track the location of the leftmost and rightmost matched statements,
   when matching a sequence a statements.

   This is not a problem for other types of patterns (expressions, ...),
   which match a single node rather than a sequence.
*)

(* Leftmost and rightmost statements matched by the pattern.
   They're used to determine the matched region when the pattern
   matches a sequence of statements.
*)
type t =
  | Empty
  | Span of {
      leftmost_stmt: AST_generic.stmt;
      rightmost_stmt: AST_generic.stmt;
    }

(* TODO: it would be safer to check that the statement indded ends to the
   right of the previous rightmost statement. *)
let extend rightmost_stmt span =
  match span with
  | Empty ->
      Span {
        leftmost_stmt = rightmost_stmt;
        rightmost_stmt;
      }
  | Span x ->
      Span { x with rightmost_stmt }

let location x =
  match x with
  | Empty -> None
  | Span {leftmost_stmt; rightmost_stmt} ->
      let min_loc, _ = Lib_AST.range_of_any (AST_generic.S leftmost_stmt) in
      let _, max_loc = Lib_AST.range_of_any (AST_generic.S rightmost_stmt) in
      Some (min_loc, max_loc)

(* This returns the first and last stmts as a list, but doesn't include
   any statement in between.
   TODO: If this is really used, we should store all the statements,
   which is complicated to do correctly with caching.
*)
let list_stmts x =
  match x with
  | Empty -> []
  | Span {leftmost_stmt; rightmost_stmt} -> [leftmost_stmt; rightmost_stmt]
