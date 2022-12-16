TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_ml-analyze

SRC= ast_ml.ml ast_ml_build.ml ml_to_generic.ml \
     highlight_ml.ml \
	 module_ml.ml \
     graph_code_ml.ml \
     tags_ml.ml \
     database_light_ml.ml \
     coverage_ml.ml \
     test_analyze_ml.ml unit_analyze_ml.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/external/ppx_deriving \
   $(TOP)/commons_ocollection \
   $(TOP)/commons_core \
   $(TOP)/globals \
   $(TOP)/external/ \
   $(TOP)/h_program-lang \
   $(TOP)/graph_code \
   $(TOP)/h_visualization  \
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

clean::
	rm -f $(TARGET).top

ast_ml.cmo: ast_ml.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show ast_ml.ml
ast_ml.cmx: ast_ml.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show ast_ml.ml
