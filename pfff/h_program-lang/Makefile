TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-h_program-lang

SRC= entity_code.ml scope_code.ml \
     flag_parsing.ml \
     Unicode.ml \
     Parse_info.ml meta_parse_info.ml \
     comment_code.ml \
     ast_fuzzy.ml meta_ast_fuzzy.ml lib_ast_fuzzy.ml \
     AST_generic.ml meta_ast_generic_common.ml \
     highlight_code.ml \
     archi_code.ml archi_code_lexer.ml archi_code_parse.ml \
     skip_code.ml \
     refactoring_code.ml lib_unparser.ml \
     database_code.ml \
     prolog_code.ml datalog_code.ml \
     layer_code.ml \
     tags_file.ml \
     Error_code.ml R2c.ml \
     layer_parse_errors.ml \
     coverage_code.ml layer_coverage.ml \
     overlay_code.ml big_grep.ml pleac.ml info_code.ml \
     pretty_print_code.ml \
     test_program_lang.ml unit_program_lang.ml

# See also graph_code/graph_code.ml! closely related to h_program-lang/

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/uutf \
 $(TOP)/external/uucp \
 $(TOP)/external/json-wheel \
 $(TOP)/h_files-format

# other sources:
#  prolog_code.pl, facts.pl, for the prolog-based code query engine

# dead: visitor_code, statistics_code, programming-language, ast_generic
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

AST_generic.cmo: AST_generic.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
AST_generic.cmx: AST_generic.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

Parse_info.cmo: Parse_info.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
Parse_info.cmx: Parse_info.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

archi_code_lexer.ml: archi_code_lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f archi_code_lexer.ml
beforedepend:: archi_code_lexer.ml
