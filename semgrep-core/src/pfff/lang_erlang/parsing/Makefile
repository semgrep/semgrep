TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_erlang

-include $(TOP)/Makefile.config

SRC= ast_erlang.ml \
     lib_parsing_erlang.ml \
     parser_erlang.ml lexer_erlang.ml \
     token_helpers_erlang.ml \
     parse_erlang.ml \
     test_parsing_erlang.ml


#     meta_ast_erlang.ml \
#     export_ast_erlang.ml \
#     visitor_erlang.ml  \
#     unparse_erlang.ml \


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


lexer_erlang.ml: lexer_erlang.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_erlang.ml
beforedepend:: lexer_erlang.ml

parser_erlang.ml parser_erlang.mli: parser_erlang.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_erlang.ml parser_erlang.mli parser_erlang.output
beforedepend:: parser_erlang.ml parser_erlang.mli
