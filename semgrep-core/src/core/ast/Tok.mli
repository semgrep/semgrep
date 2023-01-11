(*
   The token type Parse_info.t + ppx interfaces suitable for the generic AST.

   Used by more than one module (AST_generic and Raw_tree).
*)

type t = Parse_info.t [@@deriving show]

(*
   These 3 functions assume all tokens are equal. This isn't always what we want,
   so this why they're not provided by Parse_info.

   The default behavior of these functions would be obtained with:
   [@@deriving eq, hash]
*)
val equal : t -> t -> bool
val hash : t -> int
val hash_fold_t : 'acc -> t -> 'acc
