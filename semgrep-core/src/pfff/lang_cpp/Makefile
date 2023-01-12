TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_cpp

-include $(TOP)/Makefile.config

SRC= flag_parsing_cpp.ml \
     token_cpp.ml cst_cpp.ml \
     type_cpp.ml \
     visitor_cpp.ml lib_parsing_cpp.ml \
     parser_cpp_mly_helper.ml parser_cpp.ml parser_cpp2.ml lexer_cpp.ml \
     token_helpers_cpp.ml token_views_cpp.ml token_views_context.ml \
     parsing_hacks_lib.ml pp_token.ml \
     parsing_hacks_pp.ml parsing_hacks_cpp.ml parsing_hacks_typedef.ml \
     parsing_hacks_define.ml \
     parsing_hacks.ml \
     parsing_recovery_cpp.ml \
     parse_cpp.ml \
     test_parsing_cpp.ml unit_parsing_cpp.ml \

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/ppx_deriving \
 $(TOP)/external/dyp \
 $(TOP)/commons_core \
 $(TOP)/globals \
 $(TOP)/h_program-lang \

NUM_PERMITTED_CONFLICTS= 2

SCRIPTDIR= $(TOP)/scripts

MENHIRLOG= menhir_out.log

LANG= ml

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

cst_cpp.cmo: cst_cpp.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
cst_cpp.cmx: cst_cpp.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<


lexer_cpp.ml: lexer_cpp.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_cpp.ml
beforedepend:: lexer_cpp.ml

OCAMLYACC=menhir --unused-tokens --explain

parser_cpp.ml parser_cpp.mli: parser_cpp.mly
	$(OCAMLYACC) $< 2>&1 | tee $(MENHIRLOG) && $(SCRIPTDIR)/check_menhir_conflicts.sh $(MENHIRLOG) $(NUM_PERMITTED_CONFLICTS) $(LANG) parser_cpp.ml parser_cpp.mli

clean::
	rm -f parser_cpp.ml parser_cpp.mli parser_cpp.output parser_cpp.conflicts $(MENHIRLOG)
beforedepend:: parser_cpp.ml parser_cpp.mli

#TODO: should use --noemit-token-type
parser_cpp2.ml parser_cpp2.mli: parser_cpp2.dyp
	dypgen --pv-obj --ocamlc '$(INCLUDES) -w y' $<
	perl -p -i -e 's/type token =/type token = Parser_cpp.token open Parser_cpp(*/' \
         parser_cpp2.ml*
	perl -p -i -e 's/ TUnknown of.*/*)/' \
         parser_cpp2.ml*
clean::
	rm -f parser_cpp2.ml parser_cpp2.mli parser_cpp2.extract_type parser_cpp2_temp.ml

# does not work well with dypgen so have to do that below
#beforedepend:: parser_cpp.ml parser_cpp.mli
parser_cpp2.cmo: parser_cpp2.cmi
parser_cpp2.cmo: parser_cpp2.ml
	ocamlc $(INCLUDES) -w y -c $<


parsing_hacks.cmo: parsing_hacks.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_cpp.cmo: parsing_hacks_cpp.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_pp.cmo: parsing_hacks_pp.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_typedef.cmo: parsing_hacks_typedef.ml
	$(OCAMLC) -w -9 -c $<
token_views_context.cmo: token_views_context.ml
	$(OCAMLC) -w -9 -c $<
