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
	./run-benchmarks.py

# Use the latest semgrep build published on DockerHub.
.PHONY: docker
docker:
	docker pull $(DOCKER_IMG)
	./run-benchmarks.py --docker $(DOCKER_IMG)

# This is for testing the setup.
.PHONY: dummy
dummy:
	docker pull $(DOCKER_IMG)
	./run-benchmarks.py --dummy --docker $(DOCKER_IMG) --upload

.PHONY: dummy-local
dummy-local:
	./run-benchmarks.py --dummy --upload

.PHONY: clean
clean:
	rm -rf bench/*/input
	rm -rf bench/*/output
