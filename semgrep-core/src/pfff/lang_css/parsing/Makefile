TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_css

-include $(TOP)/Makefile.config

SRC= ast_css.ml \
     meta_ast_css.ml \
     lib_parsing_css.ml \
     export_ast_css.ml \
     parser_css.ml lexer_css.ml \
     token_helpers_css.ml \
     parse_css.ml \
     test_parsing_css.ml

#     visitor_html.ml \
#     unparse_html.ml \


SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma

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


lexer_css.ml: lexer_css.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_css.ml
beforedepend:: lexer_css.ml


parser_css.ml parser_css.mli: parser_css.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_css.ml parser_css.mli parser_css.output
beforedepend:: parser_css.ml parser_css.mli
