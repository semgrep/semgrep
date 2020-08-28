TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
#BIN=prettyphp

OPAMPKG=pfff-lang_php-pretty

SRC= \
  ast_pp.ml ast_pp_build.ml \
  pretty_print.ml\
  unparse_pretty_print_mix.ml \
  unit_pretty_print_php.ml

LIBS= \
  $(TOP)/commons/lib.cma \
  $(TOP)/h_program-lang/lib.cma \
  $(TOP)/globals/lib.cma \
  $(TOP)/lang_php/parsing/lib.cma \

SYSLIBS=bigarray.cma str.cma unix.cma
SYSLIBSOPT = $(SYSLIBS:.cma=.cmxa)

INCLUDEDIRS= \
  $(TOP)/commons \
  $(TOP)/globals \
  $(TOP)/h_program-lang \
  $(TOP)/lang_php/parsing \

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
# $(BIN)
all.opt:: $(TARGET).cmxa 
#$(BIN).opt

#$(BIN): $(OBJS) $(LIBS) main.ml
#	$(OCAMLC) -o $@ -custom $(SYSLIBS) $(LIBS) $(OBJS) main.ml
#$(BIN).opt: $(OBJS) $(LIBS) main.ml
#	$(OCAMLOPT) -o $@ $(SYSLIBSOPT) $(LIBS:.cma=.cmxa) $(OPTOBJS) main.ml
$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

clean::
	rm -f $(BIN) $(BIN).opt
