This used to be in ../targets/dependency_aware/perf/
but many of our tests are copying ../targets/ in /tmp
resulting in 'make e2e' consuming more than 4G in /tmp
for each run, saturating quickly your hard disk.
Hence this separate directory for big targets.
