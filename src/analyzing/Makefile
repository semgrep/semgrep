TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_GENERIC-analyze

SRC= \
  Typing.ml Naming_AST.ml Normalize_AST.ml Constant_propagation.ml \
  controlflow.ml controlflow_build.ml \
  controlflow_visitor.ml \
  lrvalue.ml \
  Dataflow.ml dataflow_reaching.ml dataflow_liveness.ml \
  IL.ml Display_IL.ml CFG.ml AST_to_IL.ml CFG_build.ml \
  Dataflow_tainting.ml \
  Test_analyze_generic.ml \
  Unit_naming_generic.ml Unit_typing_generic.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/external/ppx_deriving \
   $(TOP)/commons_core $(TOP)/commons_ocollection \
   $(TOP)/h_program-lang \
   $(TOP)/globals \
   ../parsing \

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

IL.cmo: IL.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
IL.cmx: IL.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<

clean::
	rm -f $(TARGET).top
