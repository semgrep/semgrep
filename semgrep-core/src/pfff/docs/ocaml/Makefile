
##############################################################################
# Literate Programming rules
##############################################################################

SRCNW=OCaml.tex.nw
TEXMAIN=OCaml.tex.nw
TEX=OCaml.tex

LPSRC=foo.ml

#------------------------------------------------------------------------------
SYNCFLAGS=-md5sum_in_auxfile -less_marks

SYNCWEB=~/c__syncweb/syncweb $(SYNCFLAGS)
NOWEB=~/c__syncweb/scripts/noweblatex
OCAMLDOC=ocamldoc $(INCLUDES)

PDFLATEX=pdflatex --shell-escape

#------------------------------------------------------------------------------
sync:
	for i in $(LPSRC); do $(SYNCWEB) $(SRCNW) $$i || exit 1; done
#sync -lang ?? dotemacs-ocaml.el
#sync -lang ?? Makefile.template

pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
	pdflatex $(TEX)

lpclean::
	rm -f *.aux *.toc *.log
	rm -f $(TEX)

clean:: lpclean
