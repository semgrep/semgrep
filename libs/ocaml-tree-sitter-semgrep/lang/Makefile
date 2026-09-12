#
# Build support for the programming languages listed in this folder.
#
# Public entry point: make test-grammars from the Semgrep repository root.
#

# Languages set up for local CST parse benches via lang-stat (see 'stat').
# Prefer stats/parsing-stats for Semgrep product parse metrics.
# Split into two batches because a full serial run exceeds the old 3h CI limit.
# alt: parallelize like stats/parsing-stats/run-all
STAT_LANGUAGES1 = \
  bash \
  c \
  cairo \
  cpp \
  c-sharp \
  c-sharp-pro \
  dockerfile \
  elixir \
  fga \
  go \
  hack \
  haskell \
  hcl \
  java \
  javascript \
  julia \
  kotlin

STAT_LANGUAGES2 = \
  apex \
  cfml \
  gosu \
  lua \
  move-on-sui \
  ocaml \
  php \
  powershell \
  promql \
  python \
  r \
  ruby \
  rust \
  solidity \
  tsx \
  typescript

STAT_LANGUAGES = $(STAT_LANGUAGES1) $(STAT_LANGUAGES2)

# Language directories under lang/ (excluding tooling dirs).
SUPPORTED_DIALECTS := $(filter-out semgrep-grammars,$(patsubst %/,%,$(filter %/,$(wildcard */))))

# Build and test each language. Does not run parsing stats (see 'stat:').
#
# Takes a while: for a single language, run ./test-lang instead. The language
# list is the same one .github/workflows/ocaml-tree-sitter-test-languages.yml uses for its matrix.
#
.PHONY: test
test:
	$(MAKE) -C ../../.. grammar-tools
	../core/scripts/provision-tree-sitter-all
	set -e; \
        failed=""; \
	for lang in $$(../scripts/list-languages); do \
          ./test-lang $$lang || failed="$$failed $$lang"; \
        done; \
	if [ -n "$$failed" ]; then \
	  echo "*** Failed to build or test the following languages:$$failed"; \
	  exit 1; \
	fi

.PHONY: stat
stat:
	$(MAKE) stat1
	$(MAKE) stat2

# Run local CST parse benches for each language (lang-stat).
# Each language needs projects.txt and extensions.txt.
# Per-language output: $$lang/stat/summary.txt (no cross-language rollup).
#
.PHONY: stat1
stat1:
	set -eu; \
	for lang in $(STAT_LANGUAGES1); do $(MAKE) -C $$lang stat; done

.PHONY: stat2
stat2:
	set -eu; \
	for lang in $(STAT_LANGUAGES2); do $(MAKE) -C $$lang stat; done

# Same as 'stat' but uses projects-priv.txt instead of projects.txt.
.PHONY: stat-priv
stat-priv:
	set -e; \
	for lang in $(STAT_LANGUAGES); do $(MAKE) -C $$lang stat-priv; done

.PHONY: clean
clean:
	set -e; \
	for lang in $(SUPPORTED_DIALECTS); do $(MAKE) -C $$lang clean; done
