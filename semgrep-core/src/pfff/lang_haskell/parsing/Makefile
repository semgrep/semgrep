TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_haskell

-include $(TOP)/Makefile.config

SRC= ast_hs.ml \
     parser_hs.ml lexer_hs.ml \
     parse_hs.ml \
     lib_parsing_hs.ml \
     test_parsing_hs.ml

#     unparse_js.ml \
#     meta_ast_js.ml \
#     export_ast_js.ml \
#     visitor_js.ml lib_parsing_js.ml \


SYSLIBS= str.cma unix.cma

# globals.cma is used only for Config.patch in test_parse_nw
LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
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


lexer_hs.ml: lexer_hs.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_hs.ml
beforedepend:: lexer_hs.ml
