#
# Run semgrep benchmarks.
# See './run-benchmarks --help' for options.
#

DOCKER_IMG = returntocorp/semgrep:develop

# Run whichever version of semgrep is available locally.
.PHONY: run
run:
	@set -e; \
	if ! semgrep --version > /dev/null; then \
	  echo "Missing 'semgrep' executable. Consider using 'make docker'."; \
	  exit 1; \
	fi
	./run-benchmarks

# Use the latest semgrep build published on DockerHub.
.PHONY: docker
docker:
	docker pull $(DOCKER_IMG)
	./run-benchmarks --docker $(DOCKER_IMG)

# This is for testing the setup.
.PHONY: dummy
dummy:
	docker pull $(DOCKER_IMG)
	./run-benchmarks --dummy --docker $(DOCKER_IMG) --upload

.PHONY: dummy-local
dummy-local:
	./run-benchmarks --dummy --upload

#TODO: we should not delete bench/dummy/!
.PHONY: clean
clean:
	rm -rf bench/*/input
	rm -rf bench/*/output

.PHONY: regenerate-tests
regenerate-tests:
	./run-benchmarks --config configs/ci_small_repos.yaml --std-only --save-findings-to snapshots/ci_small_repos_baseline.json
	jq . snapshots/ci_small_repos_baseline.json > snapshots/ci_small_repos_baseline.json.tmp
	mv snapshots/ci_small_repos_baseline.json.tmp snapshots/ci_small_repos_baseline.json
