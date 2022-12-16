TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ruby

-include $(TOP)/Makefile.config

#coupling: if you modify this you need to run
# make update_dot_depend
SRC= utils_ruby.ml \
  ast_ruby.ml \
  ast_ruby_helpers.ml \
  parser_ruby_helpers.ml \
  parser_ruby.ml \
  lexer_parser_ruby.ml \
  lexer_ruby.ml \
  token_helpers_ruby.ml \
  parse_ruby.ml \
  lib_parsing_ruby.ml \
  test_parsing_ruby.ml unit_parsing_ruby.ml


SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/dyp \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)
$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

ast_ruby.cmo: ast_ruby.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord  ast_ruby.ml
ast_ruby.cmx: ast_ruby.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord ast_ruby.ml


lexer_ruby.ml: lexer_ruby.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_ruby.ml
beforedepend:: lexer_ruby.ml

#TODO: switch to use --no-mli which avoid the need to
# pass the -I and other problems
parser_ruby.ml parser_ruby.mli: parser_ruby.dyp
	dypgen --pv-obj --ocamlc '$(INCLUDES) -w y' $<

#TODO: we need to keep the .mli for now because
# we get a compilation error when using dune
# because we use --no-mli, but then the inferred type
# can not be generalized because of some weak references
clean::
	rm -f parser_ruby.ml parser_ruby_temp.ml #parser_ruby.mli

#this does not work because dypgen above wants to compile
# but in make depend the .cmo are not there yet
#beforedepend:: parser_ruby.ml parser_ruby.mli
# this is ugly
include dot-depend-manual

update_dot_depend:
	make depend
	cp .depend dot-depend-manual

parser_ruby.cmo: parser_ruby.ml
	ocamlc $(INCLUDES) -w y -c $<
