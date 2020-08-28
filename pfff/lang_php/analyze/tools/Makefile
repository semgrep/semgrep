TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
     xdebug.ml \
     phpunit.ml \
     php_serialize.ml \
     xhprof.ml \

# this requires lib-xml/ to be in external/ again
#     phpmanual_xml.ml
# hphpi_trace.ml is in facebook/qa_test/ for now (more convenient)

SYSLIBS= str.cma unix.cma 
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../../parsing/lib.cma \
     ../foundation/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons_ocollection \
  $(TOP)/commons_core \
  $(TOP)/external/json-wheel \
  $(TOP)/h_program-lang \
  ../../parsing \
  ../foundation \

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


#PP=-pp camlp4o

EXTRA_FLAGS=-w -3

php_serialize.cmo: php_serialize.ml
	$(OCAMLC) $(PP) $(EXTRA_FLAGS) -c $<

php_serialize.cmx: php_serialize.ml
	$(OCAMLOPT) $(PP) $(EXTRA_FLAGS) -c $<

