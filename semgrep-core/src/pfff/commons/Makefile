##############################################################################
# Variables
##############################################################################

# Useful when commons is part of a program with a Makefile.config (e.g., pfff)
-include ../Makefile.config

#note: if you add a file (a .mli or .ml), dont forget to 'make depend'
SRC=Common.ml common2.ml \
 OCaml.ml\
 file_type.ml\
 set_.ml map_.ml \
 Dumper.ml OUnit.ml

# lib.cma
LIBNAME=lib
# ~/.opam/.../commons/lib.cma
OPAMPKG=commons

# for 'make top'
SYSLIBS=unix.cma str.cma

OCAMLMKLIB=ocamlc -a
OCAMLMKLIBOPT=ocamlopt -a
#bug? ocamlmklib, does some weird things when you actually dont have C code

-include Makefile.common

##############################################################################
# Developer rules
##############################################################################

forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt
clean::
	rm -f gmon.out

#obsolete: use codegraph instead!
dependencygraph:
	ocamldep *.mli *.ml > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

dependencygraph2:
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl -I extra > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps
