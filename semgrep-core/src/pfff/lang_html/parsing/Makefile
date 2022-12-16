TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_html

-include $(TOP)/Makefile.config

SRC= ast_html.ml \
     meta_ast_html.ml \
     visitor_html.ml \
     lib_parsing_html.ml \
     parser_html.ml lexer_html.ml \
     token_helpers_html.ml \
     dtd_simple.ml \
     parse_html.ml \
     unparse_html.ml \
     pretty_print_html.ml \
     unit_parsing_html.ml \
     test_parsing_html.ml


SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/globals \
 $(TOP)/h_program-lang

# $(TOP)/external/ocamlnet/netsys \
# $(TOP)/external/ocamlnet/netstring \

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


lexer_html.ml: lexer_html.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_html.ml
beforedepend:: lexer_html.ml


parser_html.ml parser_html.mli: parser_html.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_html.ml parser_html.mli parser_html.output
beforedepend:: parser_html.ml parser_html.mli
