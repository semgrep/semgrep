TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-h_files-format

SRC= outline.ml simple_format.ml source_tree.ml

LIBS=$(TOP)/commons/lib.cma
INCLUDEDIRS= $(TOP)/commons

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa
opt:: all.opt
$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)
