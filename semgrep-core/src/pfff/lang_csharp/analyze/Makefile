TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_csharp-analyze

SRC= \
     highlight_csharp.ml \
     database_light_csharp.ml \
     test_analyze_csharp.ml

-include $(TOP)/Makefile.config

#ifeq ($(FEATURES_PCRE), 1)
#PCRECMA=pcre.cma
#PCREINCLUDE= +pcre
#PCREINCLUDE=$(TOP)/
#endif

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

MAKESUBDIRS=

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons_ocollection \
   $(TOP)/external/ \
   $(TOP)/h_program-lang \
   $(TOP)/globals \
   ../parsing \

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common


##############################################################################
# Top rules
##############################################################################
all:: rec $(TARGET).cma
all.opt:: rec.opt $(TARGET).cmxa

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done
depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done


$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


##############################################################################
# Literate Programming rules
##############################################################################
