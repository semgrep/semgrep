TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ml

-include $(TOP)/Makefile.config

SRC= cst_ml.ml \
     visitor_ml.ml lib_parsing_ml.ml \
     parser_ml.ml lexer_ml.ml \
     token_helpers_ml.ml \
     parse_ml.ml \
     test_parsing_ml.ml unit_parsing_ml.ml

SYSLIBS= str.cma unix.cma

# globals.cma is used only for Config.patch in test_parse_ml
LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

NUM_PERMITTED_CONFLICTS= 0

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= ml


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

# to see the actual expanded source, use -dsource instead of -c below
cst_ml.cmo: cst_ml.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show cst_ml.ml

cst_ml.cmx: cst_ml.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show cst_ml.ml

clean::
	rm -f $(TARGET).top

lexer_ml.ml: lexer_ml.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_ml.ml
beforedepend:: lexer_ml.ml

OCAMLYACC=menhir --unused-tokens --explain --fixed-exception

parser_ml.ml parser_ml.mli: parser_ml.mly
	  $(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_ml.ml parser_ml.mli

clean::
	rm -f parser_ml.ml parser_ml.mli parser_ml.output $(MENHIRLOG)
beforedepend:: parser_ml.ml parser_ml.mli
