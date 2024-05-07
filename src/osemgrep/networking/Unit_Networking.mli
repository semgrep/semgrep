(* OCaml's TLS implementation, whether bindings to OpenSSL or the pure ocaml
 * version sometimes breaks. Hours gone and we don't know why.
 * These are some tests to try to catch it.
 *)
val tests : < Cap.network ; .. > -> Testo.t list
