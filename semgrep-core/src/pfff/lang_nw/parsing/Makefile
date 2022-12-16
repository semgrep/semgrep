TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_nw

-include $(TOP)/Makefile.config

SRC= ast_nw.ml \
     lexer_nw.ml \
     token_helpers_nw.ml \
     parse_nw.ml \
     test_parsing_nw.ml

#     unparse_js.ml \
#     meta_ast_js.ml \
#     export_ast_js.ml \
#     visitor_js.ml lib_parsing_js.ml \


INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

# globals.cma is used only for Config.patch in test_parse_nw
LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

SYSLIBS= str.cma unix.cma

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


lexer_nw.ml: lexer_nw.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_nw.ml
beforedepend:: lexer_nw.ml
