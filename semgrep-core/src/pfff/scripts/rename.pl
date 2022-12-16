#!/usr/bin/perl -pi.orig

#ex: git grep Ocaml -l | grep -e '.ml$' | xargs ./scripts/rename.pl

# could also use spatch!

#s/\bOcaml\b/OCaml/g;
s/\bAst_generic\b/AST_generic/g;
