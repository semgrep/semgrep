TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= graphe.ml

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/ocamlgraph/ \

OPAMPKG=pfff-commons-graph

#needed for make top
LIBS=../../external/ocamlgraph/graph.cma
SYSLIBS= str.cma unix.cma

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
