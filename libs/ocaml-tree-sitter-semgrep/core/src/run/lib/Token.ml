(*
   A token, i.e. a string with its location in the file where it comes from.
*)

type t = Loc.t * string

let sexp_of_t (_loc, tok) =
  Sexplib.Sexp.Atom tok
