TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_python

-include $(TOP)/Makefile.config

SRC= Flag_parsing_python.ml AST_python.ml \
     Visitor_python.ml \
     Lib_parsing_python.ml \
     Parser_python.ml Lexer_python.ml \
     Token_helpers_python.ml \
     Parsing_hacks_python.ml Parse_python.ml \
     Test_parsing_python.ml Unit_parsing_python.ml

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \

NUM_PERMITTED_CONFLICTS= 0

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= python

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

AST_python.cmo: AST_python.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
AST_python.cmx: AST_python.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

Lexer_python.ml: Lexer_python.mll
	$(OCAMLLEX) $<
clean::
	rm -f Lexer_python.ml
beforedepend:: Lexer_python.ml


OCAMLYACC=menhir --unused-tokens --explain --fixed-exception

Parser_python.ml Parser_python.mli: Parser_python.mly
	$(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_python.ml parser_python.mli

clean::
	rm -f Parser_python.ml Parser_python.mli $(MENHIRLOG) Parser_python.output
beforedepend:: Parser_python.ml Parser_python.mli
