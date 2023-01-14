# Build executables and place them where semgrep expects them.
# These are normally copied by '/cli/setup.py' but it doesn't happen if we
# run only 'dune build'.
#
# This is for development purposes only as I'm not sure if a symlink is ok
# for packaging things up on the Python side.
#
# Usage:
#  $ make dev
#  $ PIPENV_PIPFILE=~/semgrep/cli/Pipfile pipenv run semgrep ...
#
.PHONY: dev
dev:
	$(MAKE) all
	rm -f ../cli/src/semgrep/bin/semgrep-core
	ln -s ../../../../semgrep-core/bin/semgrep-core \
	  ../cli/src/semgrep/bin/semgrep-core
	rm -f ../cli/src/semgrep/bin/osemgrep
	ln -s ../../../../semgrep-core/bin/osemgrep \
	  ../cli/src/semgrep/bin/osemgrep
	rm -f ../cli/src/semgrep/bin/semgrep_bridge_core.so
	ln -s ../../../../semgrep-core/bin/semgrep_bridge_core.so \
	  ../cli/src/semgrep/bin/semgrep_bridge_core.so
	rm -f ../cli/src/semgrep/bin/semgrep_bridge_python.so
	ln -s ../../../../semgrep-core/bin/semgrep_bridge_python.so \
	  ../cli/src/semgrep/bin/semgrep_bridge_python.so

# Run matching performance tests
.PHONY: perf
perf:
	@echo "--- default settings ---"
	cd ./perf && ./run-perf-suite
	@echo "--- no caching ---"
	cd ./perf && ./run-perf-suite --no-cache
	@echo "--- maximum caching ---"
	cd ./perf && ./run-perf-suite --max-cache

# Run matching performance tests and post them to the semgrep dashboard
# at https://dashboard.semgrep.dev/
#
# This is meant for CI, which hopefully runs on similar machines each time.
#
.PHONY: report-perf
report-perf:
	cd ./perf && ./run-perf-suite --upload
