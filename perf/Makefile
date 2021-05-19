#
# Run semgrep benchmarks.
# See './run-benchmarks --help' for options.
#

DOCKER_IMG = returntocorp/semgrep:develop

setup-env:
	pip install pipenv \
    pipenv install

# Run whichever version of semgrep is available locally.
.PHONY: run
run: setup-env
	@set -e; \
	if ! semgrep --version > /dev/null; then \
	  echo "Missing 'semgrep' executable. Consider using 'make docker'."; \
	  exit 1; \
	fi
	pipenv run ./run-benchmarks

# Use the latest semgrep build published on DockerHub.
.PHONY: docker
docker: setup-env
	docker pull $(DOCKER_IMG)
	pipenv run ./run-benchmarks --docker $(DOCKER_IMG)

# This is for testing the setup.
.PHONY: dummy
dummy: setup-env
	docker pull $(DOCKER_IMG)
	pipenv run ./run-benchmarks --dummy --docker $(DOCKER_IMG) --upload

.PHONY: dummy-local
dummy-local: setup-env
	pipenv run ./run-benchmarks --dummy --upload

.PHONY: clean
clean:
	rm -rf bench/*/input
	rm -rf bench/*/output
