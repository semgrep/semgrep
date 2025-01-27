(*
   Compile a pattern into a regexp.
*)
[@@@alert "-deprecated"]

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)
[@@deriving show, eq]

type metavariable = {
  kind : metavariable_kind;
  bare_name : string; (* 'X', not '$X', not '$...X' *)
}
[@@deriving show, eq]

type t = private {
  pcre : Pcre_.t;
  metavariable_groups : (int * metavariable) list;
}
[@@deriving show, eq]

(* Shortcut for all parsing + compilation *)
val from_string : Conf.t -> string -> t

(* Convert a metavariable to concrete semgrep syntax e.g. '$X' or '$...X' *)
val string_of_metavariable : metavariable -> string
