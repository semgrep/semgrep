(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * Dumper.mli 1.1 2005/02/03 23:07:47 rich Exp
 *)

(* Dump any OCaml data-structure in a string. dump() relies on the Obj module
 * internally so it is limited (e.g., it just dumps numbers for constructors).
 * You should use instead 'deriving show' which correctly handle
 * constructors, fields, etc. However, if you can't use 'deriving show',
 * then this function helps.
 *)
val dump : 'a -> string
