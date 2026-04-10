## [1.158.0](https://github.com/semgrep/semgrep/releases/tag/v1.158.0) - 2026-04-09

### ### Added

- Added support for a supply chain hook for the Semgrep Plugin (supply-chain-hook)
- Computing taint configs, ~1/4-1/2 of the semgrep-core time in interfile scans, is now done in parallel according to the number of jobs (ENGINE-2649)
- Semgrep Pro interfile engine (--pro) taint analysis has been redesigned, significantly improving performance (estimated 20-40% improvement). This improvement introduces a slight change in how findings are generated, that may result in more true positives, or less false positives. To revert to previous behavior, pass `--no-x-run-taint-once` as a flag. (engine-2468)

### ### Changed

- semgrep-core macOS binaries are now dynamically linked to the system's libraries. (macos-binary-build)
- semgrep-core manylinux binaries are now dynamically linked to the system's glibc on glibc systems. This introduces a minimum glibc version requirement of >=2.35, which is satisfied in Ubuntu >=22.04, Debian >=12, RHEL >=10, and other glibc distributions with at least glibc 2.35. Linux systems running an older glibc will need to upgrade their OS. (manylinux-binary-build)
- The manylinux wheel is now tagged as manylinux_2_35_<arch>, reflecting a minimum
  requirement of glibc version 2.35. (manylinux-wheel-tag)
- semgrep-core musllinux binaries are now dynamically linked to the system's musl libc on musl systems. (musllinux-binary-build)
- The musllinux PyPI wheel is now tagged as musllinux_1_2_<arch>, reflecting a requirement
  of musl libc version 1.2. (musllinux-wheel-tag)
- The LSP and MCP servers now use the v2 config download endpoint by default when fetching rules from Semgrep AppSec Platform. Set `SEMGREP_DISABLE_CONFIG_DOWNLOAD_V2=1` to fall back to the legacy endpoint. (SMS-2284)

### ### Fixed

- Fixed IDE login issues where network errors during token verification were incorrectly clearing the saved token. The LSP now distinguishes 401 Unauthorized (invalid token) from other errors (e.g. network failures), surfacing appropriate messages instead. (ide-login)
- Fixed SARIF taint trace output: step locations now use the correct file URI, and the full taint sink call trace is included in `codeFlows`. (engine-2570)
- The --x-mem-policy flag now propagates to the RPC subprocess, fixing memory tuning for dependency resolution and other RPC-based operations. (pylon-20772)
