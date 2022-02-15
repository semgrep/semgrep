(*
   Detect regexps vulnerable to denial-of-service attacks (ReDoS) by static
   analysis.
*)

(* Return whether the given string is vulnerable regexp.
   Return None if the string can't be parsed as a regexp. *)
val regexp_may_explode : string -> bool option
