# Run 'make setup' to install the correct versions of the dependencies.
# Run 'make' to regenerate all files.
#
# You must run 'make' after editing an atd file. This will update
# other files that we keep under version control. Naturally, don't edit
# generated files by hand.

VER=v1

# Those files are in lowercase because atdpy/atdts seems to
# generate lowercase files, even though the input is capitalized
FILES= \
  semgrep_output_$(VER)_t.ml \
  semgrep_output_$(VER)_t.mli \
  semgrep_output_$(VER)_j.ml \
  semgrep_output_$(VER)_j.mli \
  semgrep_output_$(VER).py \
  semgrep_output_$(VER).ts \
  semgrep_output_$(VER).jsonschema \
  semgrep_output_$(VER).proto \
  Language.ml \
  Language.mli \
  lang.json \
  semgrep_metrics.py

# Regenerate all files.
#
# If someone accidentally touches a generated file, the lazy file regeneration
# based on timestamps won't happen. This is really only a problem because
# we keep the generated files, with their timestamp, in git.
# To avoid such issues, force-build is now the default target.
#
.PHONY: force-build
force-build:
	$(MAKE) clean
	$(MAKE) build

.PHONY: build
build: $(FILES)

# need atdpy >= 3.0.0 to translate <doc text=...> into Python docstrings
# need atdpy >= 2.12.0 for semgrep_metric.py
# need atdpy >= 2.11.0 to support parametrized types
%.py: %.atd
	opam exec -- atdpy $<

# -j-defaults is for producing '"field" = []' instead of omitting the field
# if it's defined as '~field: item list'.
# This allows us to produce the same JSON output with pysemgrep and osemgrep
# since atdpy keeps it simple and will always output '"field" = []'.
%_j.ml %_j.mli: %.atd
	opam exec -- atdgen -j -j-std -j-defaults $<

%_t.ml %_t.mli: %.atd
	opam exec -- atdgen -t $<

# need atdts >= 2.13.0
%.ts: %.atd
	opam exec -- atdts $<

# need atdcat >= 2.6.0
semgrep_output_$(VER).jsonschema: semgrep_output_$(VER).atd
	opam exec -- atdcat -jsonschema cli_output $< > $@

semgrep_output_$(VER).proto: semgrep_output_$(VER).jsonschema
	scripts/jsonschema2protobuf.py $< semgrep_output_$(VER) > $@

# The call to ocamlc is just to typecheck the generated OCaml files
Language.ml Language.mli lang.json: generate.py
	uvx mypy generate
	./generate
	opam exec -- ocamlc -o Language Language.mli Language.ml

.PHONY: clean
clean:
	rm -f $(FILES) Language

.PHONY: dev-setup
dev-setup:
	opam switch create semgrep-interfaces-dev 5.3.0
	$(MAKE) setup

# This takes a while but ensures we use the correct versions of the atd tools.
.PHONY: setup
setup:
	opam install -y --deps-only ./semgrep-interfaces.opam
	jq --version > /dev/null || (echo "jq is required to run the tests" && exit 1)

.PHONY: test-backwards-compatibility
test-backwards-compatibility:
	opam exec -- ./scripts/check-backwards-compatibility

.PHONY: test-schema-validation
test-schema-validation:
	uv run ./scripts/validate.py rule_schema_v1.yaml tests/jsonschema/rules

# putting these files in their own dir is a workaround so we don't have to
# delete __init__.py. running mypy normally doesn't work since the folder name
# has a - in it which isn't allowed
# TODO? rename the repo to semgrep_interfaces to avoid this hack?
.PHONY: typecheck
typecheck:
	mkdir -p typecheck
	ln -s ../semgrep_output_v1.py typecheck/semgrep_output_v1.py
	ln -s ../semgrep_metrics.py typecheck/semgrep_metrics.py
	uvx mypy@1.19.1 -p typecheck

# The tests require semgrep-core, among other things.
.PHONY: test
test: test-backwards-compatibility test-schema-validation typecheck
