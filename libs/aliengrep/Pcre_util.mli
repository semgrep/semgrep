(*
   PCRE-related code used both for parsing patterns and for scanning targets.
*)

(* Return a PCRE-compatible character class from a list of characters.
   (only supports ASCII characters)
*)
val char_class_of_list : char list -> string
