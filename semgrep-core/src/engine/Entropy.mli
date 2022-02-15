(*
   A special built-in string analyzer that estimates whether a string
   looks like a secret.

   The entropy is an estimate of the number of bits of information in
   a string. For some application we want a high total entropy. For others,
   we care more about the entropy per character or per bit of input.
*)

(* Whether the string is estimated to have higher entropy. *)
val has_high_entropy : ?min_entropy:int -> string -> bool
