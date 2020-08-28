VERSION = $(shell grep ';; Version:' tuareg.el \
	| sed 's/;; Version: *\([0-9.]\+\).*/\1/')
DESCRIPTION = $(shell grep ';;; tuareg.el ---' tuareg.el \
	| sed 's/[^-]*--- *\(.*\)/\1/')
REQUIREMENTS = $(shell grep ';; Package-Requires:' tuareg.el \
	| sed 's/;; Package-Requires: *\(.\+\).*/\1/')
DIST_NAME = tuareg-$(VERSION)

ELS = tuareg.el ocamldebug.el
ELC = $(ELS:.el=.elc)

DIST_FILES += $(ELS) Makefile README

EMACS = emacs

#ENABLE_SMIE = --eval '(setq tuareg-use-smie t)'
RM = rm -rf
CP = cp -f
LN = ln
DIFF = diff -u -B

INSTALL_RM_R = $(RM)
INSTALL_MKDIR = mkdir
INSTALL_CP = $(CP)

all : elc tuareg-site-file.el

elc : $(ELC)

%.elc : %.el
	$(EMACS) -batch $(NOINIT) -f batch-byte-compile $<

# ifneq ($(realpath .hg),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = hg id -i | fgrep -v '+' >/dev/null || \
#         (echo 'uncommitted changes' >&2; exit 1); \
#         hg id -i --debug > $(VERSION_FILE)
# else
# ifneq ($(realpath .svn),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = svn info | grep Revision: | sed 's/Revision: //' > $(VERSION_FILE)
# else
# ifneq ($(realpath .bzr),)
# POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
# MAKE_VERSION_FILE = bzr log -l -1 | grep revno: > $(VERSION_FILE)
# else
# ifneq ($(realpath $(VERSION_FILE)),)
# POST_INSTALL_HOOK =
# MAKE_VERSION_FILE = @echo "Using \"$(VERSION_FILE)\" in the distribution."
# else
# POST_INSTALL_HOOK =
# MAKE_VERSION_FILE = @(echo "missing \"$(VERSION_FILE)\" in the distribution?" >&2; exit 1)
# endif
# endif
# endif
# endif

# install : $(ELC) $(VERSION_FILE)
#         fgrep `cat $(VERSION_FILE)` tuareg.elc >/dev/null 2>&1 || \
#          ($(RM) tuareg.elc; $(MAKE) tuareg.elc)
#         $(INSTALL_RM_R) ${DEST}
#         $(INSTALL_MKDIR) ${DEST}
#         for f in $(ELS) $(ELC) $(VERSION_FILE); do $(INSTALL_CP) $$f $(DEST)/$$f; done
#         $(POST_INSTALL_HOOK)


.PHONY: refresh
refresh:

check : sample.ml.test

%.test: % $(ELC) refresh
	@echo ====Indent $*====
	-$(RM) $@
	$(EMACS) --batch -q --no-site-file $(ENABLE_SMIE) \
	  --load tuareg.elc $< \
	  --eval '(setq indent-tabs-mode nil)' \
	  --eval '(defun ask-user-about-lock (file opponent) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(write-region (point-min) (point-max) "$@")'
	$(DIFF) $< $@ || true

tuareg-site-file.el: refresh
	echo "\
	;;; $@ --- Automatically extracted autoloads.\n\
	;;; Code:\n\
	(add-to-list 'load-path\n\
	             (or (file-name-directory load-file-name) (car load-path)))\n\
	" >$@
	$(EMACS) --batch --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

.PHONY: dist tar
dist: $(DIST_NAME).tar.gz
tar: $(DIST_NAME).tar

$(DIST_NAME).tar.gz $(DIST_NAME).tar: $(DIST_FILES)
	mkdir -p $(DIST_NAME)
	for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
	echo '(define-package "tuareg" "$(VERSION)" "$(DESCRIPTION)" ' "'"'$(REQUIREMENTS))' > $(DIST_NAME)/tuareg-pkg.el
	tar acvf $@ $(DIST_NAME)
	$(RM) -rf $(DIST_NAME)

clean :
	$(RM) $(ELC) "$(DIST_NAME).tar.gz" "$(DIST_NAME).tar"
#         $(POST_INSTALL_HOOK)

# .PHONY : all elc clean install force check distrib dist
