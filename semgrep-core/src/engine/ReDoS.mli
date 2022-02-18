(*
   Determine whether a regexp is evil i.e. susceptible to take a very
   long time to match on some short input.

   Detecting this is useful for preventing regex(p) denial-of-service
   attacks (ReDoS).
   https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS
*)

(* Return whether the given string is vulnerable regexp.
   Return None if the string can't be parsed as a regexp. *)
val regexp_may_be_vulnerable :
  ?dialect:Pfff_lang_regexp.Dialect.t -> string -> bool option

(* Exposed only for testing purposes *)
val unquote : string -> string
