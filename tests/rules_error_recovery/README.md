This directory contains tests that may trigger errors at runtime
(e.g., some parsing error on some target files) but semgrep should
still recover from those errors and find matches.

We can't put those tests in ../rules/ because there we are more
strict and ensure that there are no error such as partial
parsing of targets.
