(* Caching computation (e.g., parsed ASTs, network data) on disk *)

(* take file from which computation is done, an extension, and the function
 * and will compute the function only once and then save result in
 * file ^ extension.
 *)
val cache_computation :
  ?use_cache:bool ->
  Common.filename ->
  string (* extension *) ->
  (unit -> 'a) ->
  'a

(* a more robust version where the client describes the dependencies of the
 * computation so it will relaunch the computation in 'f' if needed.
 *)
val cache_computation_robust :
  Common.filename ->
  string (* extension for marshalled object *) ->
  Common.filename list * 'x ->
  string (* extension for marshalled dependencies *) ->
  (unit -> 'a) ->
  'a
