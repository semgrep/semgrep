(*
   Fast check that tells whether a pattern has any chance of matching the
   document with 'Match.search'.

   This checks that all the required literals in the pattern exist somewhere
   in the document.
*)

(*
   If this returns false, it's guaranteed that 'Match.search' would return
   no matches.
*)
val may_match : case_sensitive:bool -> Pattern_AST.t -> Doc_AST.t -> bool
