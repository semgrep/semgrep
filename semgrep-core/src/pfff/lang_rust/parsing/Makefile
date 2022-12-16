TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_rust

-include $(TOP)/Makefile.config

SRC= ast_rust.ml \
     lib_parsing_rust.ml \
     parser_rust.ml lexer_rust.ml \
     token_helpers_rust.ml \
     parse_rust.ml \
     test_parsing_rust.ml


#     meta_ast_rust.ml \
#     export_ast_rust.ml \
#     visitor_rust.ml  \
#     unparse_rust.ml \


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

lexer_rust.ml: lexer_rust.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_rust.ml
beforedepend:: lexer_rust.ml

parser_rust.ml parser_rust.mli: parser_rust.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_rust.ml parser_rust.mli parser_rust.output
beforedepend:: parser_rust.ml parser_rust.mli
