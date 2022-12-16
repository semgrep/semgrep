TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-lang_php-analyze

include $(TOP)/Makefile.config

ifeq ($(FEATURE_BDB), 1)
BDBINCLUDE=$(TOP)/external/ocamlbdb
endif

#todo? have a visual/ directory, where put highlight, htmlize, dependencies?
SRC= test_analyze_php.ml \

#pcre deps
#     htmlize_php.ml \

-include $(TOP)/Makefile.config

#old:
# emacs_mode.ml
# relation.ml

#ifeq ($(FEATURE_PCRE), 1)
#PCRECMA=pcre.cma
#PCREINCLUDE= +pcre
#PCREINCLUDE=$(TOP)/
#endif

# h_version-control is used for now only by deadcode_php.ml

SYSLIBS= str.cma unix.cma $(PCRECMA)
LIBS=$(TOP)/commons/lib.cma \
     $(TOP)/h_program-lang/lib.cma \
     ../parsing/lib.cma \

MAKESUBDIRS=foundation tools checker \
  qa_test visual \
#qa_code annotaters
#typing finder

INCLUDEDIRS= $(TOP)/commons \
   $(TOP)/external/ppx_deriving \
   $(TOP)/commons_ocollection \
   $(TOP)/commons_core \
   $(TOP)/external/json-wheel \
   $(TOP)/h_program-lang \
   $(TOP)/globals \
   $(BDBINCLUDE) \
   ../parsing \
   foundation \
   checker \
   tools \
   qa_test \

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

TEXMAIN=Analyze_php.tex.nw
TEX=Analyze_php.tex

# set of noweb (.nw) files

# must be in the same order of the #include for syncweb multi files support
# to work
SRC_ORIG=Analyze_php.tex.nw Analyze_php_intro.tex.nw Analyze_php_use.tex.nw \
 Analyze_php_implem.tex.nw \
 foundation/controlflow_php.ml.nw \
 tools/xdebug.ml.nw \
 static_analysis/code_rank_php.ml.nw \
 static_analysis/cyclomatic_php.ml.nw \
 static_analysis/deadcode_php.ml.nw \
 foundation/builtins_php.ml.nw \
 tools/phpunit.ml.nw \
 static_analysis/coverage_static_php.ml.nw \
 dynamic_analysis/coverage_dynamic_php.ml.nw \
 allmli.mli.nw \
 test_analyze_php.ml.nw \
 test_analyze_db_php.ml.nw \

# typing/typing_trivial_php.ml.nw \
# foundation/pil.ml.nw \

# static_analysis/tainted_php.ml.nw \

#foundation/dataflow_php.ml.nw
# static_analysis/dataflow_php_array.ml.nw \


# set of ml files, the views. Also used by make lpclean.

# TODO later do via a filter-out
SRC_ML= \
       foundation/controlflow_php.ml foundation/controlflow_build_php.ml \
       foundation/builtins_php.ml \
       static_analysis/coverage_static_php.ml \
       tools/xdebug.ml \
       tools/phpunit.ml \
       static_analysis/code_rank_php.ml \
       static_analysis/cyclomatic_php.ml \
       static_analysis/deadcode_php.ml \
       dynamic_analysis/coverage_dynamic_php.ml \
       test_analyze_php.ml \
       test_analyze_db_php.ml \

#       foundation/pil.ml foundation/pil_build.ml \
#       foundation/dataflow_php.ml static_analysis/dataflow_php_liveness.ml \
#       typing/typing_trivial_php.ml \
#       static_analysis/tainted_php.ml \
#       static_analysis/dataflow_php_array.ml \

SRC_MLI=$(SRC_ML:ml=mli)

SRC_NO_MLI=flag_analyze_php.ml
#TODO, mli files without their .ml counterpart included in the document
SRC_MLI_NO_ML_NW= \
 foundation/entity_php.mli \
 foundation/lib_analyze_php.mli \
 foundation/namespace_php.mli \
 foundation/comment_php.mli \
 foundation/annotation_php.mli \
 foundation/aliasing_function_php.mli \
 foundation/bottomup_analysis_php.mli \
 foundation/callgraph_php.mli \
 foundation/graph_php.mli \
 foundation/include_require_php.mli \
 foundation/database_php.mli \
 database_php_build.mli \
 database_php_build2.mli \
 database/database_php_query.mli \
 database/database_php_statistics.mli \
 static_analysis/statistics_php.mli \

# annotaters/comment_annotater_php.mli \
# annotaters/info_annotater_php.mli \

# typing/type_annotater_php.mli \
# typing/typing_php.mli \
# typing/typing_weak_php.mli \


SRC_MLI+=$(SRC_MLI_NO_ML_NW)

SRC_VIEWS=$(SRC_MLI) $(SRC_ML)

#------------------------------------------------------------------------------
sync:
	for i in $(SRC_VIEWS); do $(SYNCWEB) $(SRC_ORIG) $$i || exit 1; done

sync2:


pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
	pdflatex $(TEX)

lpclean::
	rm -f $(TEX)

lpdistclean::
	rm -f $(SRC_VIEWS) .md5sum_* $(TEX)

clean::
	rm -f *.aux *.toc *.log *.brf *.out



# edit the resulting dot file and remove the _php in the names of the modules
doti2:
#	$(OCAMLDOC) $(SRC_MLI) -dot -dot-colors white
#	perl -p -i -e 's/\[style=filled, color=white\]//;' ocamldoc.out
	dot -Tps ocamldoc.out > dot.ps
	gv dot.ps
	mv dot.ps Fig_graph_mli.ps
#	ps2pdf Fig_graph_mli.ps


DIRS= $(filter-out foo, $(MAKESUBDIRS))
DSRC+=$(DIRS:=/*.ml)
DSRC+=$(wildcard *.ml)

dotall:
	ocamldoc -I +threads $(INCLUDES) $(DSRC)  -dot -dot-reduce
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps
