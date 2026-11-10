## [1.185.0](https://github.com/semgrep/semgrep/releases/tag/v1.185.0) - 2026-11-10

### ### Added

- Secrets: emit structured, privacy-safe telemetry for custom validator HTTP requests (hostname, port, status, latency, rule/scan correlation) (SMS-3122)

### ### Changed

- The `semgrep/semgrep` Docker image no longer includes the OpenSSH server (`sshd`) or `sftp-server`. It now installs only the OpenSSH client (`ssh`, `scp`, `ssh-keyscan`, `ssh-agent`, `ssh-add` and `ssh-keygen`), which is all git needs to fetch over SSH. Container scanners will no longer report sshd-only CVEs against the image. (docker-ssh-client-only)
- julia grammar update (v0.25.0):

  - Julia source parsing and rule patterns now support public API declarations while distinguishing them from export declarations and calls named public.
  - Julia source parsing and rule patterns now support outer-qualified loop bindings, preserving their distinction from ordinary bindings and a variable named outer.
  - Julia parsing and matching now support typed const fields without initializers in mutable structs.
  - Julia source parsing and rule patterns now support renaming imported macros, such as import A: @m as @n and import A: @m as @B.n, and keep the @ on macro names in imports, exports, and public declarations.
  - Julia rules can now match unary ± expressions and distinguish binary ± from ∓, reducing operator-related false positives.
  - Julia numeral juxtaposition such as 1__2 is now represented as multiplication by __2 and can be found by multiplication patterns.
  - Julia function-definition patterns now retain where constraints, avoiding matches against definitions with a different constraint.

  (julia-v0.25.0)

### ### Fixed

- OCaml: taint analysis now follows data through expression statements and
  sequence expressions. For example, in `let x = (r := 1; source ()) in sink x`,
  the value of the sequence is the result of `source ()`, so the finding on
  `sink x` is now reported. Taint also no longer leaks from one `match` case
  into the next one. (ocaml-expression-statements)
- OCaml: the last expression of a function body is now treated as the function's result, so taint flows through calls. (ocaml-implicit-returns)
- On Windows, Semgrep now runs programs it calls by name, such as git and pysemgrep, only from fully qualified PATH directories. Previously Windows checked the current directory first, so an executable committed to a scanned repository could run during a scan. Initially reported by Aikido Security. (CODE-10122)
