include Int64
open Ppx_hash_lib.Std.Hash.Builtin
open Sexplib.Std

(* There isn't an available hash_fold or hash function in Int64.t, so we have to
   make it up here.
   Unfortunately, there doesn't seem to be a direct hash function for int64 in
   the Ppx_hash_lib either, so we just write what it would be.
*)
let hash_fold_int64 = Ppx_hash_lib.Std.Hash.fold_int64

type t = int64 [@@deriving hash, show, eq, sexp]

let rec power x n = if equal n 0L then 1L else mul x (power x (sub n 1L))
let ( + ) = Int64.add
let ( - ) = Int64.sub
let ( * ) = Int64.mul
let ( / ) = Int64.div
let ( mod ) = Int64.rem
let ( asr ) = Int64.shift_right
let ( lsl ) = Int64.shift_left
let ( lor ) = Int64.logor
let ( land ) = Int64.logand
let ( lxor ) = Int64.logxor
let ( =|= ) = Int64.equal
let ( > ) i1 i2 = Int64.compare i1 i2 > 0
let ( < ) i1 i2 = Int64.compare i1 i2 < 0
let ( >= ) i1 i2 = Int64.compare i1 i2 >= 0
let ( <= ) i1 i2 = Int64.compare i1 i2 <= 0
