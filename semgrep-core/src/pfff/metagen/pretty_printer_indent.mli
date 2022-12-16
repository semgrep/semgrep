
(* could be a abstract type *)
type env = {
  fn : int -> string -> unit;
  p : string -> unit;
  i : int;
  nl : unit -> unit;
  dbg : bool;
}

val indent : env -> env
val indent_fn : env -> (env -> 'a) -> 'a

val list_iter_indent : env -> (env -> 'a -> unit) -> 'a list -> unit
val hashtbl_iter_indent :
  env -> (env -> 'a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit

val may : ('a -> unit) -> 'a option -> unit
val must : ('a -> 'b) -> 'a option -> 'b

val init_printer : ?msg:string option -> ?debug:bool -> out_channel -> env

val pfn : env -> ('a, unit, string, unit) format4 -> 'a
val dbg : env -> ('a, unit, string, unit) format4 -> 'a

val ( --> ) : env -> (env -> 'a) -> 'a
val ( += ) : env -> ('a, unit, string, unit) format4 -> 'a
val ( -= ) : env -> ('a, unit, string, unit) format4 -> 'a
val ( $ ) : ('a -> 'b) -> 'a -> 'b
