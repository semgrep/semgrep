(*
   PCRE-related code used both for parsing patterns and for scanning targets.
*)

(* Return a PCRE-compatible character class from a list of characters.
   (only supports ASCII characters)

   If contents_only is set to true, the enclosing brackets are omitted.
*)
val char_class_of_list : ?contents_only:bool -> char list -> string

(*
   Produce a pattern that matches a sequence of characters literally.

   You must use this instead of Pcre.quote if you're using the `EXTENDED flag.
   It's safe to always use this instead of Pcre.quote.
*)
val quote : string -> string
