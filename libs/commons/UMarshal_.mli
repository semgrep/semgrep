(* !! UNSAFE !!
 *
 * at least add a version number in 'a and marshall a pair (version, data)
 * in write_value(), so at least after get_value() you can double
 * check that the data can still be read.
 *)
val get_value : Fpath.t -> 'a
val write_value : 'a -> Fpath.t -> unit
