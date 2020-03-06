TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_php-matcher

SRC= flag_matcher_php.ml   \
     refactoring_code_php.ml \
     metavars_php.ml \
     php_vs_php.ml \
     matching_php.ml \
     transforming_php.ml \
     sgrep_php.ml \
     spatch_php.ml \
     unit_matcher_php.ml \

SYSLIBS= str.cma unix.cma
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons_ocollection \
   $(TOP)/h_program-lang $(TOP)/h_version-control  \
   $(TOP)/globals \
   $(TOP)/external/ \
   ../parsing

##############################################################################
# Generic variables
##############################################################################
#WARNING_FLAGS=-w +A-4-29-6-45-32
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

#BUG in ocamlc, false positive on unused value
php_vs_php.cmo: php_vs_php.ml
	$(OCAMLC) -w -A -c $<
