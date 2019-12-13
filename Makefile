PROGS=\
 sgrep spatch \

 matcher/lib.cma \


    matcher/lib.cma \

  matcher \

  lang_php/matcher/lib.cma \

     lang_php/matcher/lib.cma \

   lang_php/matcher \

INSTALL_SUBDIRS= \
matcher

#------------------------------------------------------------------------------
# sgrep/spatch targets
#------------------------------------------------------------------------------

sgrep: $(BASICLIBS) $(OBJS) main_sgrep.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
sgrep.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_sgrep.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

spatch: $(BASICLIBS) $(OBJS) main_spatch.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
spatch.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_spatch.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^


