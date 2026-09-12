(*
   Abbreviations for common words, to be used in generated names.
   These are meant to be better than truncating a word at an arbitrary
   offset.
*)

(* Abbreviate the ascii words found in the input string, using known
   abbreviations such as 'exp' for 'expression' when possible,
   otherwise truncating words to 'trunc_len'.

   's' is used to indicate plural, without consideration for English grammar,
   it is ignored for the purpose of word length, and is preserved in the
   abbreviated word.

   All-caps words are not abbreviated.

   max_len = 6, trunc_len = 4:

     statement  -> stmt       (* hardcoded abbreviation *)
     statements -> stmts      (* same, pluralized *)
     badger     -> badger     (* at max length *)
     giraffe    -> gira       (* over max length, must be truncated *)
     giraffes   -> giras      (* over max length even without the 's' *)
     plumbus    -> plumbus    (* because it's considered "plural" *)
     CHUPACABRA -> CHUPACABRA (* because it's considered very important
                                 or already abbreviated *)
*)
val words : ?max_len:int -> ?trunc_len:int -> string -> string
