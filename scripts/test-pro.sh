#! /usr/bin/env bash

cp /root/semgrep-core-proprietary /usr/local/bin/semgrep-core-proprietary

chmod +x /usr/local/bin/semgrep-core-proprietary

cd /root

semgrep --config "p/default-v2" . --interfile
