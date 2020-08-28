TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_php-analyze-checker

# mostly local analysis
SRC= \
  error_php.ml suggest_fix_php.ml \
  check_includes_php.ml \
  check_lint_php.ml \
  check_misc_php.ml \
  check_variables_php.ml \
  check_cfg_php.ml \
  check_micro_clones_php.ml
#  check_dfg_php.ml \

# use some global analysis via entity_finder
SRC+= check_functions_php.ml check_classes_php.ml

SRC+= check_all_php.ml layer_checker_php.ml
SRC+= unit_checker_php.ml
SRC+= auto_fix_php.ml
SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
  $(TOP)/external/ppx_deriving \
  $(TOP)/commons_ocollection \
   $(TOP)/external/ \
   $(TOP)/h_program-lang \
   $(TOP)/graph_code \
   $(TOP)/globals \
   ../../parsing \
   .. \
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
