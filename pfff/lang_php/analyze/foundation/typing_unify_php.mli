
(* return the unified type of two types and modify env.subst by side effects *)
val unify: 
  Env_typing_php.env ->
  Env_typing_php.t -> Env_typing_php.t ->
  Env_typing_php.t
