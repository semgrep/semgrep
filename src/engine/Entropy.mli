(*
   A special built-in string analyzer that estimates whether a string
   looks like a secret.

   The entropy is an estimate of the number of bits of information in
   a string, based on statistics for the language.
   For some application we want a high total entropy. For others,
   we care more about the entropy per character or per bit of input.
*)

(* The entropy i.e. the number of bits of information in the string,
   as estimated internally. Maybe don't expose this to the user.
   See 'relative_entropy'. *)
val entropy : string -> float

(* The number of bits of information per bit of data.
   This is the entropy divided by the length of the string in bits.
   It is usually smaller than 1 but it can be greater than 1 if for example,
   the string is made of a repetition of the rarest character in the alphabet.
   [TODO: check if there's a standard term for this]
*)
val information_density : string -> float

(*
   For a string to have a high score, it must have both:
   - high entropy (number of bits of information, grows with string length)
   - high information density (bits of information / bits of data)

   The score is nonnegative. A high score is 2.
   This is designed to detect secret keys encoded in Base64 or hexadecimal.
*)
val score : string -> int
val has_high_score : string -> bool

(* Set in Data_init.init() from data in Entropy_data.ml *)
val english_trigrams_ref : (string * int) array ref
