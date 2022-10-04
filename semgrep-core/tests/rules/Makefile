
# We convert the .jsonnet in .yaml here, even though semgrep-core
# knows how to do that by itself, to avoid to add a jsonnet dependency
# in CI.
JSONNET_TESTS=\
  metavar_cond.jsonnet \
  metavar_cond2.jsonnet \
  metavar_cond_octal.jsonnet \
  metavar_regex.jsonnet \
  negation_ajin.jsonnet \
  regexp.jsonnet \
  regexp_nomatch.jsonnet \

all: $(JSONNET_TESTS:.jsonnet=.yaml)

%.json: %.jsonnet lib_semgrep.jsonnet
	jsonnet $< > $@

# this requires json-yaml of https://github.com/sjmulder/json-yaml
# (available in arch-linux at https://aur.archlinux.org/packages/json-yaml/)
%.yaml: %.json
	json-yaml $^ > $@



