include module type of Int64

type t = Int64.t [@@deriving hash, show, eq, sexp]

val power : int64 -> int64 -> int64

(* This module exists to be opened locally, so that normal arithmetic
   operations on 64-bit integers can be used with familiar operators.
*)

val ( + ) : int64 -> int64 -> int64
val ( - ) : int64 -> int64 -> int64
val ( * ) : int64 -> int64 -> int64
val ( / ) : int64 -> int64 -> int64
val ( mod ) : int64 -> int64 -> int64
val ( asr ) : int64 -> int -> int64
val ( lsl ) : int64 -> int -> int64
val ( lor ) : int64 -> int64 -> int64
val ( land ) : int64 -> int64 -> int64
val ( lxor ) : int64 -> int64 -> int64
val ( =|= ) : int64 -> int64 -> bool
val ( > ) : int64 -> int64 -> bool
val ( < ) : int64 -> int64 -> bool
val ( >= ) : int64 -> int64 -> bool
val ( <= ) : int64 -> int64 -> bool
