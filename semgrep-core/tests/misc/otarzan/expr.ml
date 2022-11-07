type expr =
  | Int of int
  | String of string
  | Plus of expr * expr
  (* with tarzan *)
