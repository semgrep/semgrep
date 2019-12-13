TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-matcher

SRC= flag_matcher.ml \
     matching_report.ml \
     \
     metavars_fuzzy.ml fuzzy_vs_fuzzy.ml \
     matching_fuzzy.ml transforming_fuzzy.ml \
     sgrep_fuzzy.ml spatch_fuzzy.ml \
     \
     metavars_generic.ml generic_vs_generic.ml \
     sgrep_generic.ml \
     \
     unit_matcher.ml

#todo: refactoring_code_fuzzy.ml

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma 

INCLUDEDIRS= $(TOP)/commons $(TOP)/commons_ocollection \
   $(TOP)/external/json-wheel \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ \
   $(TOP)/lang_GENERIC/parsing \
   $(TOP)/lang_GENERIC/analyze \

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
