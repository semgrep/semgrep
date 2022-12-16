TOP=..
#############################################################################
# Configuration section
#############################################################################
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib
OPAMPKG=pfff-h_visualization

SRC= figures.ml simple_color.ml \
     plot_jgraph.ml \
     treemap.ml

#need use json library in external/, or could be guarded by FEATURE_JSON
# treemap_json.ml
#need put back xml library in external/
#graph_gephi.ml

#todo: empty for now
SRC+=gnuplot.ml html.ml latex.ml #dot.ml

LIBS=$(TOP)/commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \

SYSLIBS= bigarray.cma str.cma unix.cma

# graphics is not included by default because it can conflict with
# applications using lablgtk
ifeq ($(FEATURE_GRAPHICS),1)
SRC2+=graphics/treemap_graphics.ml
SYSLIBS+=graphics.cma
PROGS+=treemap_viewer
endif

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(PROGS)
all.opt: $(TARGET).cmxa $(PROGS:=.opt)
$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

treemap_viewer: $(LIBS) $(EXTRALIBS) $(OBJS) $(SRC2:.ml=.cmo) graphics/main_treemap.cmo
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $^

treemap_viewer.opt: $(LIBS:.cma=.cmxa) $(EXTRALIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) graphics/main_treemap.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBSOPT) $(SYSLIBS:.cma=.cmxa) $^

clean::
	rm -f $(PROGS) $(PROGS:=.opt)

##############################################################################
# Literate Programming rules
##############################################################################

# must be in the same order of the #include for syncweb multi files support
# to work
SRCNW=Treemap.tex.nw
TEXMAIN=Treemap.tex.nw
TEX=Treemap.tex

# user oriented, public API in some sense + ml that share code with mli
LPSRC1=treemap.mli treemap.ml treemap_json.mli treemap_json.ml \
 graphics/treemap_graphics.mli graphics/treemap_graphics.ml \
 graphics/main_treemap.ml
LPSRCSHARED=
# internal oriented
LPSRC2=
LPSRC=$(LPSRC1) $(LPSRCSHARED) $(LPSRC2)

#------------------------------------------------------------------------------
sync:
	for i in $(LPSRC); do $(SYNCWEB) $(SRCNW) $$i || exit 1; done

pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
#	pdflatex $(TEX)

lpclean::
	rm -f $(TEX)

lpclean2::
	rm -f $(TEX) $(LPSRC) $(LPSRCOTHER) .md5sum_*

clean::
	rm -f *.aux *.toc *.log
