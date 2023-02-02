#! /usr/bin/env bash

semgrep install-semgrep-pro

semgrep --config "p/deepsemgrep" . --interfile
