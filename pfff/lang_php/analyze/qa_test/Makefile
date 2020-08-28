TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
  coverage_tests_php.ml \
  unit_coverage_php.ml \

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons_ocollection \
  $(TOP)/external/json-wheel \
  $(TOP)/h_program-lang \
  ../../parsing \
  .. \
  ../foundation \
  ../tools \
  $(TOP)/globals \

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


