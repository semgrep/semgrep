TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_js-analyze

SRC= \
  ast_js.ml visitor_ast_js.ml map_ast_js.ml \
  js_to_generic.ml \
  lib_analyze_js.ml \
  transpile_js.ml ast_js_build.ml \
  class_pre_es6.ml module_pre_es6.ml comment_js.ml annotation_js.ml \
  module_path_js.ml \
  stdlib_js.ml graph_code_js.ml \
  highlight_js.ml \
  database_light_js.ml \
  tags_js.ml \
  utils_js.ml \
  test_analyze_js.ml unit_analyze_js.ml

#jil.ml

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

#database \
# dynamic_analysis static_analysis \
# qa_test qa_code \

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/external/ppx_deriving \
   $(TOP)/external/json-wheel \
   $(TOP)/commons_ocollection $(TOP)/commons_core \
   $(TOP)/h_program-lang \
   $(TOP)/globals \
   $(TOP)/graph_code \
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

ast_js.cmo: ast_js.ml
	ocamlfind ocamlc -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<
ast_js.cmx: ast_js.ml
	ocamlfind ocamlopt -c $(OCAMLCFLAGS) $(INCLUDES) -package ppx_deriving.show $<


#temporary
TEMP=abstract_interpreter_js_XX
$(TEMP).cmo: $(TEMP).ml
	$(OCAMLC) -w -A -c $<
