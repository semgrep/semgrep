(*
   Match a compiled pattern against a target string.
*)

(*
   Position and length of a matched substring, in bytes.
   To recover line/column information, see how it's done
   in Xpattern_match_regexp.ml.
*)
type loc = {
  start : int;
  length : int;
  (* The matched data. This is redundant but convenient for testing. *)
  substring : string;
}
[@@deriving show]

type match_ = {
  match_loc : loc;
  captures : (Pat_compile.metavariable * loc) list;
}
[@@deriving show]

(* For debugging *)
val show_matches : match_ list -> string

(* Search for matches in a target string. *)
val search : Pat_compile.t -> string -> match_ list
