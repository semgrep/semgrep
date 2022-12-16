TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= ast_web.ml \
     parse_web.ml \
     test_parsing_web.ml \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma

INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/globals \
 $(TOP)/h_program-lang \
 $(TOP)/lang_html/parsing \
 $(TOP)/lang_js/parsing \
 $(TOP)/lang_css/parsing \

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

##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
