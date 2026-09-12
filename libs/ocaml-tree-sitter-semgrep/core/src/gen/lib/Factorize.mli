(*
   Identify duplicated anonymous rules and give them a name.
*)

(*
   Identify grammar nodes that are duplicated or exist as a named rule
   and share a name.

   The default parameters are for detecting duplicates of sufficient size,
   resulting in the creation of named nodes used multiple times:
   - create_names = true
   - min_uses = 2
   - min_size = 3

   This function can also be used to identify nodes that are the result
   of inlining, and replacing them by their original name. The settings
   for this would be:
   - create_names = false
   - min_uses = 0
   - min_size = 0
*)
val factorize_rules :
  ?create_names:bool ->
  ?min_uses:int ->
  ?min_size:int ->
  CST_grammar.t -> CST_grammar.t
