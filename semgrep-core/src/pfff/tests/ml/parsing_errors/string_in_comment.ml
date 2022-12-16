(* the closing * ) in the comment below is not recognized
 * as such by ocamlc, because it's inside a string. My
 * parser does not do that currently. This is questionable
 * whether it's a good idea.
*)

(* CHECK
   let nv_re = Pcre.regexp "^([a-zA-Z0-9_.]+)(=(.*))?$"
*)
let nv_re = Pcre.regexp "^([^=;]+)(=(.*))?$"
