(*
   There are (too) many places in Semgrep where we define a "location"
   and "position".
   A position is usually a place in a file, like a cursor.
   A location is usually a region in this file.

   Here are example of places where we define a loc (and often pos):
    - Tok.location (a Pos.t and a string)
    - Tok_range.t (a pair of Tok.t)
    - semgrep_output_v1.location (a file and start/end positions)
    - Spacegrep.Loc.t (using a range of Lexing.position)
    - Aliengrep.Match.loc (no filename, just start x length)
    - Tree_sitter_run.Loc.t (no filename either, just start/end positions)

   TODO: we should factorize some of those locations, in the mean
   time we define Loc.t as Tok.location
*)

(* alias to the most common location type used in Semgrep *)
type t = Tok.location [@@deriving show]
