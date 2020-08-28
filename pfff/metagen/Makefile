TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-meta

BIN=gen_code

SRC= ast_js_tof.ml ast_generic_tof.ml \
  pretty_printer_indent.ml \
  gen_ml_code.ml \
  test_meta.ml


-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     $(TOP)/lang_js/parsing/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/commons_ocollection $(TOP)/commons_core \
   $(TOP)/h_program-lang  \
   $(TOP)/globals \
   $(TOP)/graph_code \
   $(TOP)/lang_js/parsing \
   $(TOP)/lang_js/analyze \

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common


##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(BIN)
all.opt:: $(TARGET).cmxa

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

$(BIN): $(LIBS) $(OBJS) main_$(BIN).cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $^
