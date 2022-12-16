TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_java

SRC= ast_java.ml \
     visitor_java.ml \
     lib_parsing_java.ml \
     parser_java.ml lexer_java.ml \
     token_helpers_java.ml \
     parsing_hacks_java.ml \
     parse_java.ml \
     test_parsing_java.ml unit_parsing_java.ml \

SYSLIBS= str.cma unix.cma

LIBS= $(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons_core \
  $(TOP)/external/ppx_deriving \
  $(TOP)/external/json-wheel \
  $(TOP)/h_program-lang \
  $(TOP)/globals

NUM_PERMITTED_CONFLICTS= 0

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= java

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

ast_java.cmo: ast_java.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
ast_java.cmx: ast_java.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<


lexer_java.ml: lexer_java.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_java.ml
beforedepend:: lexer_java.ml

OCAMLYACC=menhir --unused-tokens --explain --fixed-exception

parser_java.ml parser_java.mli: parser_java.mly
	$(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_java.ml parser_java.mli

clean::
	rm -f parser_java.ml parser_java.mli parser_java.output $(MENHIRLOG)
beforedepend:: parser_java.ml parser_java.mli
