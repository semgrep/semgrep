
# This file is intentionnaly not using ../Makefile.common.
# This is to show how to build a simple Makefile from scratch
# to people who wants to use pfff in their own application.
# update: with OPAM now being almost standard, this file is less useful.
#  just use ocamlfind as described in section "Compilation once installed"

all::

##############################################################################
# Variables
##############################################################################

include ../Makefile.config

LIBS= ../commons/lib.cma ../commons_core/lib.cma \
      ../external/ppx_deriving/ppx_deriving_runtime.cma \
      ../external/uucp/uucp.cma ../external/uutf/uutf.cma \
      ../h_program-lang/lib.cma \
      ../lang_php/parsing/lib.cma

INCLUDEDIRS=../commons \
            ../h_program-lang ../lang_php/parsing

SYSLIBS= str.cma unix.cma bigarray.cma

# If you want to embed pfff in your own application, uncomment the following
# instead of the previous settings
#MAKESUBDIRS=commons parsing_php
#LIBS=commons/lib.cma parsing_php/parsing_php.cma
#INCLUDEDIRS=commons/ parsing_php/
#rec::
#	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done
#clean::
#	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean || exit 1; done

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS=-g -dtypes # -w A

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES) -thread

##############################################################################
# Top rules
##############################################################################

# some of the ml files are generated from the literate document in parsing_php/
# so dont forget to do a 'make sync' in parsing_php/ before doing make
# in this directory.
PROGS=php_transducer.byte \
 show_function_calls1.byte show_function_calls2.byte show_function_calls3.byte\
 show_function_class_names.byte show_function_class_names2.byte \
 simple_code_search.byte \
 analyze_chain.byte

all:: $(PROGS)
opt: $(PROGS:.byte=.opt)
all.opt: opt

.SUFFIXES: .byte .opt

%.byte: %.cmo
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $(LIBS) $^

%.opt: %.cmx
	$(OCAMLOPT) -o $@ $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $^

clean::
	rm -f *.opt *.byte

##############################################################################
# Compilation once installed
##############################################################################

LIBS2= \
 $(shell ocamlfind query pfff-commons) \
 $(shell ocamlfind query pfff-h_program-lang) \
 $(shell ocamlfind query pfff-lang_js) \

STUBDIR=$(shell ocamlfind query pfff-commons)/../stublibs \

# -custom so that don't need to find the dlllib.so
test_install: pfff_linking.ml
	ocamlc -custom $(SYSLIBS) $(LIBS2:%=%/lib.cma) $(LIBS2:%=-I %) -I $(STUBDIR) pfff_linking.ml -o pfff_linking

test_install.opt: pfff_linking.ml
	ocamlopt $(SYSLIBS:%.cma=%.cmxa) $(LIBS2:%=%/lib.cmxa) $(LIBS2:%=-I %) -I $(STUBDIR) pfff_linking.ml -o pfff_linking.opt

clean::
	rm -f pfff_linking

##############################################################################
# Generic ocaml rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[iox] *.o *.annot
clean::
	rm -f *~ .*~ *.exe #*#

distclean::
	rm -f .depend

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
