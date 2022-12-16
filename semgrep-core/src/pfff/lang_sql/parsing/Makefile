TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= ast_sql.ml \
     parser_sql.ml lexer_sql.ml \
     token_helpers_sql.ml parse_sql.ml \
     test_parsing_sql.ml

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma $(TOP)/h_program-lang/lib.cma
INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/h_program-lang

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


lexer_sql.ml: lexer_sql.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_sql.ml
beforedepend:: lexer_sql.ml


parser_sql.ml parser_sql.mli: parser_sql.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_sql.ml parser_sql.mli parser_sql.output
beforedepend:: parser_sql.ml parser_sql.mli


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
