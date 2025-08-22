(*
   Encode and decode strings to hexadecimal.
*)

type t = private string
(** A lowercase hex-encoded string, represented as such in memory. *)

val compare : t -> t -> int
val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit

val of_hex_string_opt : string -> t option
(** Validate and normalize *)

val to_hex_string : t -> string
(** Return the normalized representation (lowercase hexadecimal).
    [to_hex_string x] is equivalent to [(x :> string)] *)

val encode : string -> t
(** Encode an arbitrary string *)

val decode : t -> string
(** Decode a valid hex-encoded string *)
