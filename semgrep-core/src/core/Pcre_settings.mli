(*
   Shared settings for using the Pcre module (pcre-ocaml library).
*)

(*
   To be used instead of Pcre.regexp. Refer to the Pcre documentation
   for usage.
   https://mmottl.github.io/pcre-ocaml/api/pcre/Pcre/index.html#val-regexp

   This takes care of setting PCRE_EXTRA_MATCH_LIMIT and
   PCRE_EXTRA_MATCH_LIMIT_RECURSION to the same value across platforms.
*)
val regexp :
  ?study:bool ->
  ?iflags:Pcre.icflag ->
  ?flags:Pcre.cflag list ->
  ?chtables:Pcre.chtables ->
  string ->
  Pcre.regexp

(*
   Register printers for the Pcre module/library so that exceptions show up
   nicely with 'Printexc.to_string' e.g. 'Pcre.Error(RecursionLimit)'
   instead of 'Pcre.Error(5)'.

   See issue https://github.com/mmottl/pcre-ocaml/issues/24
*)
val register_exception_printer : unit -> unit
