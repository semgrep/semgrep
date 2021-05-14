module Str_engine : sig
  type t = string * Str.regexp

  val show : t -> string

  val regexp : string -> t

  val matching_exact_string : string -> t

  val run : t -> string -> bool
end

module Pcre_engine : sig
  type t = string * Pcre.regexp

  val show : t -> string

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  (* will quote special chars in the string *)
  val matching_exact_string : string -> t

  (* add the \b around the quoted string *)
  val matching_exact_word : string -> t

  val run : t -> string -> bool
end

module Re_engine : sig
  type t = string * Re.t

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val matching_exact_string : string -> t

  val regexp : string -> t

  (* nice! *)
  val alt : t -> t -> t

  val run : t -> string -> bool
end
