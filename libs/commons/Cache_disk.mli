(* Caching computation (e.g., parsed ASTs, network data) on disk *)

(* this record is marshalled on disk *)
type ('v, 'x) cached_value_on_disk = {
  (* Extra data useful for checking the cache (e.g., a version).
   * This is useful because the Marshall module is not type-safe;
   * if the OCaml data structure types change between the moment the
   * value was cached and the moment it is retrieved, you can
   * get a segmentation fault after when starting to use the value.
   *)
  extra : 'x;
  (* the actual cached value *)
  value : 'v;
}

type ('input, 'value, 'extra) cache_methods = {
  (* the containing directory will be automatically created if needed *)
  cache_file_for_input : 'input -> Fpath.t;
  (* add some extra information with the value to enable certain checks
   * when getting back the value from the cache (e.g., version check).
   *)
  cache_extra_for_input : 'input -> 'extra;
  (* Returns true if everything is fine and the cached value is valid.
   * Otherwise the value will be recomputed and saved again in the cache.
   *)
  check_extra : 'extra -> bool;
  (* for debugging purpose, to add the input in the logs *)
  input_to_string : 'input -> string;
}

(* The first parameter is the actual computation we want to cache *)
val cache :
  ('input -> 'value) ->
  ('input, 'value, 'extra) cache_methods ->
  'input ->
  'value

(* Need unit to avoid value restriction in impl. would be nice to not need somehow? *)
val cache_lwt :
  ('input -> 'value Lwt.t) ->
  ('input, 'value, 'extra) cache_methods ->
  'input ->
  'value Lwt.t

(* deprecated old functions, less flexible than cache() above. *)

(* take file from which computation is done, an extension, and the function
 * and will compute the function only once and then save result in
 * file ^ extension.
 *)
val cache_computation :
  ?use_cache:bool ->
  string (* filename *) ->
  string (* extension *) ->
  (unit -> 'a) ->
  'a

(* a more robust version where the client describes the dependencies of the
 * computation so it will relaunch the computation in 'f' if needed.
 *)
val cache_computation_robust :
  string (* filename *) ->
  string (* extension for marshalled object *) ->
  string (* filename *) list * 'x ->
  string (* extension for marshalled dependencies *) ->
  (unit -> 'a) ->
  'a
