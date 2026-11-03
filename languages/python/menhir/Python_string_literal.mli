(** Python string-literal escape decoding, shared by ordinary and interpolated
    strings. Unicode names and aliases come from the installed Unicode database. *)

val decode : prefix:string -> string -> (string, string) result
(** Decode the content between quotes. Raw prefixes preserve the input, bytes
    prefixes leave Unicode escapes literal, and malformed escapes return an error. *)
