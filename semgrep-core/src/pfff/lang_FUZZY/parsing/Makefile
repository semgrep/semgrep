TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_FUZZY

-include $(TOP)/Makefile.config

SRC= lang_fuzzy.ml \
     parse_fuzzy.ml \
     test_parsing_fuzzy.ml

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/commons_core \
  $(TOP)/globals \
  $(TOP)/h_program-lang \
  $(TOP)/lang_python/parsing \
  $(TOP)/lang_js/parsing \
  $(TOP)/lang_c/parsing \
  $(TOP)/lang_cpp/parsing \
  $(TOP)/lang_java/parsing \
  $(TOP)/lang_ml/parsing \
  $(TOP)/lang_php/parsing \
  $(TOP)/lang_skip/parsing \
  $(TOP)/lang_go/parsing \


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
