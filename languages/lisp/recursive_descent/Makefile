TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_lisp

-include $(TOP)/Makefile.config

SRC= ast_lisp.ml \
     ast_scheme.ml \
     parser_lisp.ml lexer_lisp.ml \
     parse_lisp.ml \
     lib_parsing_lisp.ml \
     test_parsing_lisp.ml

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

lexer_lisp.ml: lexer_lisp.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_lisp.ml
beforedepend:: lexer_lisp.ml
