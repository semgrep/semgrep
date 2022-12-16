TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_csharp

-include $(TOP)/Makefile.config

SRC= ast_csharp.ml \
     lib_parsing_csharp.ml \
     parser_csharp.ml lexer_csharp.ml \
     token_helpers_csharp.ml \
     parse_csharp.ml \
     test_parsing_csharp.ml


#     meta_ast_csharp.ml \
#     export_ast_csharp.ml \
#     visitor_csharp.ml  \
#     unparse_csharp.ml \


SYSLIBS= str.cma unix.cma

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


lexer_csharp.ml: lexer_csharp.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_csharp.ml
beforedepend:: lexer_csharp.ml


parser_csharp.ml parser_csharp.mli: parser_csharp.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_csharp.ml parser_csharp.mli parser_csharp.output
beforedepend:: parser_csharp.ml parser_csharp.mli
