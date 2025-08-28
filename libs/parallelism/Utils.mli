(** Exposes some functionality available in [commons], split out
 * here in order to avoid some circular dependencies between that
 * library and this one. *)

val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
