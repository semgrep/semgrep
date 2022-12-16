TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_c

-include $(TOP)/Makefile.config

SRC= ast_c.ml visitor_c.ml \
     ast_c_build.ml \
     lib_parsing_c.ml \
     parse_c.ml \
     test_parsing_c.ml unit_parsing_c.ml \

SYSLIBS= str.cma unix.cma

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma \
 $(TOP)/lang_cpp/parsing/lib.cma

INCLUDEDIRS= $(TOP)/commons\
 $(TOP)/external/ppx_deriving \
 $(TOP)/globals \
 $(TOP)/h_program-lang \
 $(TOP)/lang_cpp/parsing \

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

ast_c.cmo: ast_c.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
ast_c.cmx: ast_c.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
