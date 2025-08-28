(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Determine whether a regexp is evil i.e. susceptible to take a very
   long time to match on some short input.

   Detecting this is useful for preventing regex(p) denial-of-service
   attacks (ReDoS).
   https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS
*)

(* Return whether the given string is a vulnerable regexp:
   - Error() indicates that the string can't be parsed as a regexp.
   - Ok(list) gives the list of subpatterns that are predicted to be
     vulnerable to ReDoS attacks.
*)
val find_vulnerable_subpatterns :
  ?dialect:Parser_regexp.Dialect.t -> string -> (string list, unit) result

(*
   Return true iff the regexp could be parsed and it looks vulnerable to
   ReDoS attacks.
*)
val regexp_may_be_vulnerable :
  ?dialect:Parser_regexp.Dialect.t -> string -> bool
