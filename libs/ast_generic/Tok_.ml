(*
   The token type Parse_info.t + ppx interfaces suitable for the generic AST.
*)

type t = Parse_info.t [@@deriving show]

(* sgrep: we do not care about position when comparing for equality 2 ASTs.
 * related: Lib_AST.abstract_position_info_any and then use OCaml generic '='.
 *)
let equal _t1 _t2 = true
let hash _t = 0
let hash_fold_t acc _t = acc
