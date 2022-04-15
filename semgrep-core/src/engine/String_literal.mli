(*
   This module is a hack for evaluating string literals.
*)

(*
   Evaluate unquoted, escaped string literal contents.

   abc\\\'\"def  ->  abc\'"def

   Assumes:

   \\ -> \
   \' -> '
   \" -> "

   Other escape sequences are left untouched.

   HACK!

   Different languages use different escaping rules for their string
   literals. This is incorrect but can be useful until each language
   parser exposes unescaped strings.
*)
val approximate_unescape : string -> string
