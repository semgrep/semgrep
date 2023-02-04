#! /usr/bin/env bash

cp ./semgrep-core-proprietary /usr/local/bin/semgrep-core-proprietary

semgrep --config "p/deepsemgrep" src --interfile
