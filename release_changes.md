## [1.120.0](https://github.com/semgrep/semgrep/releases/tag/v1.120.0) - 2025-04-22


### Added


- Added a few new entries in the .semgrepignore default file
  (e.g., _cargo, _opam, .svn) (semgrepignore)
- Add an experimental option `--x-semgrepignore-filename` to change the name of `.semgrepignore` files to something else. This can be used to scan a subproject in a separate semgrep invocation as the rest of the containing project. (semgrepignore-filename)


### Fixed


- Fixed bug in pro package-lock.json parsing where dependencies with no specified version would cause an exception (SC-2150)
- Fixed the default `-j` setting so as to take into account the cgroup
  CPU quota on Linux. This will affect Docker and other containerized
  environments that share resources on the same host. Use the new command
  `semgrep show resources --experimental` to show the default setting. (saf-1950)
