TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-graph_code

SRC= graph_code.ml graph_code_opti.ml \
     dependencies_matrix_code.ml dependencies_matrix_build.ml \
     graph_code_tags.ml graph_code_database.ml graph_code_prolog.ml \
     graph_code_class_analysis.ml graph_code_helpers.ml \
     graph_code_checker.ml \
     graph_code_export.ml \
     layer_graph_code.ml \
     test_graph_code.ml unit_graph_code.ml

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/commons_core \
 $(TOP)/commons_wrappers/graph \
 $(TOP)/external/ocamlgraph \
 $(TOP)/external/json-wheel \
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
