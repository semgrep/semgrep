(*
   Convert a grammar to OCaml type definitions.
*)

(*
   Generate files CST_$lang.ml and Parse_$lang.ml

   'lang' must be the 'underscore' version of the language name.
   Valid examples include 'c', 'c_sharp', not 'C', not 'c-sharp'.
*)
val ocaml : ?out_dir:string -> lang:string -> CST_grammar.t -> unit
