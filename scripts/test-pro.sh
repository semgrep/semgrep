#! /usr/bin/env bash

cp /root/semgrep-core-proprietary /usr/local/bin/semgrep-core-proprietary

chmod +x /usr/local/bin/semgrep-core-proprietary

semgrep --config "p/deepsemgrep" /root --interfile
