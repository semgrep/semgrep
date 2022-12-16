TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_skip

-include $(TOP)/Makefile.config

SRC= ast_skip.ml \
     meta_ast_skip.ml \
     visitor_skip.ml lib_parsing_skip.ml \
     parser_skip.ml lexer_skip.ml \
     token_helpers_skip.ml \
     parse_skip.ml \
     test_parsing_skip.ml unit_parsing_skip.ml

SYSLIBS= str.cma unix.cma

# globals.cma is used only for Config.patch in test_parse_skip
LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
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

lexer_skip.ml: lexer_skip.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_skip.ml
beforedepend:: lexer_skip.ml

parser_skip.ml parser_skip.mli: parser_skip.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_skip.ml parser_skip.mli parser_skip.output
beforedepend:: parser_skip.ml parser_skip.mli
