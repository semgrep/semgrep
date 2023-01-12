TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_go

-include $(TOP)/Makefile.config

SRC= ast_go.ml \
     visitor_go.ml \
     lib_parsing_go.ml \
     parser_go.ml lexer_go.ml \
     token_helpers_go.ml \
     parsing_hacks_go.ml \
     parse_go.ml \
     test_parsing_go.ml unit_parsing_go.ml

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

NUM_PERMITTED_CONFLICTS=0

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= go

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


ast_go.cmo: ast_go.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
ast_go.cmx: ast_go.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

lexer_go.ml: lexer_go.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_go.ml
beforedepend:: lexer_go.ml

OCAMLYACC=menhir --unused-tokens --explain --fixed-exception

parser_go.ml parser_go.mli: parser_go.mly
	$(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_go.ml parser_go.mli
clean::
	rm -f parser_go.ml parser_go.mli parser_go.output
beforedepend:: parser_go.ml parser_go.mli
