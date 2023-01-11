TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_js

-include $(TOP)/Makefile.config

SRC= flag_parsing_js.ml \
     cst_js.ml \
     visitor_js.ml lib_parsing_js.ml \
     parser_js.ml lexer_js.ml \
     token_helpers_js.ml \
     parsing_hacks_js.ml \
     parse_js.ml \
     test_parsing_js.ml unit_parsing_js.ml

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/external/json-wheel \
  $(TOP)/h_program-lang \

NUM_PERMITTED_CONFLICTS= 2

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= js

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

cst_js.cmo: cst_js.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
cst_js.cmx: cst_js.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

lexer_js.ml: lexer_js.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_js.ml
beforedepend:: lexer_js.ml

OCAMLYACC=menhir --unused-tokens --explain --fixed-exception

parser_js.ml parser_js.mli: parser_js.mly
	  $(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_js.ml parser_js.mli

clean::
	rm -f parser_js.ml parser_js.mli parser_js.output $(MENHIRLOG)
beforedepend:: parser_js.ml parser_js.mli
