(* Provide different equality over the generic AST constructs *)

type busy_with_equal = Not_busy | Structural_equal | Syntactic_equal

val busy_with_equal : busy_with_equal ref

(* used in AST_generic [equal] attribute for id_info *)
val equal_id_info : ('a -> 'a -> bool) -> 'a -> 'a -> bool

(* [with_structural_equal myfunc ast1 ast2] will call [myfunc]
 * to compare [ast1] and [ast2] with Structural_equal set in busy_with_equal
 * to perform structural equality.
 *)
val with_structural_equal : ('a -> 'a -> bool) -> 'a -> 'a -> bool

(* Similar to [with_structural_equal] but with Syntactic_equal set *)
val with_syntactic_equal : ('a -> 'a -> bool) -> 'a -> 'a -> bool
