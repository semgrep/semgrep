all:: simple_refactoring

##############################################################################
# Simple compilation
##############################################################################

# This is intentionnaly not using OCAMLC, SYSLIBS and so on to show a simple
# way to compile a pfff "script"
simple_refactoring: simple_refactoring.ml
	ocamlc -g -o simple_refactoring \
         -I ../commons -I ../lang_php/parsing -I ../h_program-lang \
          str.cma unix.cma bigarray.cma \
	  ../commons/lib.cma ../h_program-lang/lib.cma ../matcher/lib.cma \
          ../lang_php/parsing/lib.cma \
          simple_refactoring.ml
clean::
	rm -f simple_refactoring

