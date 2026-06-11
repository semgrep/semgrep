(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
val ( lsr ) : int64 -> int -> int64
val ( lor ) : int64 -> int64 -> int64
val ( land ) : int64 -> int64 -> int64
val ( lxor ) : int64 -> int64 -> int64
val ( =|= ) : int64 -> int64 -> bool
val ( <|> ) : int64 -> int64 -> bool
val ( > ) : int64 -> int64 -> bool
val ( < ) : int64 -> int64 -> bool
val ( >= ) : int64 -> int64 -> bool
val ( <= ) : int64 -> int64 -> bool
