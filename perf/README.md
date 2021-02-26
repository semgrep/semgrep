Semgrep benchmarks
==

This folder is for running realistic benchmarks for semgrep, as
opposed to more focused tests.

Requirements
--

The `semgrep` command must be available, as well as generic development
tools including git and python3.

Architecture
--

Each benchmark has a name. For each benchmark, we run the standard
semgrep commands as well as variants which disable or enable certain
optimizations.

The workspace looks like this:
```
.
├── bench
│   ├── dummy
│   │   ├── input
│   │   │   └── dummy
│   │   │       ├── rules
│   │   │       │   └── exec.yaml
│   │   │       └── targets
│   │   │           ├── hello.js
│   │   │           └── malformed.js
│   │   └── prep
│   └── njs
│       ├── input
│       │   ├── juice-shop/ (lots of files)
│       │   └── njsscan/ (lots of files)
│       └── prep
├── Makefile
├── README.md
└── run-benchmarks
```

The total duration of each benchmark is uploaded to the [semgrep
dashboard](https://dashboard.semgrep.dev/metrics), for example as
`semgrep.bench.njs.std.total-time`. Other
metrics such as memory usage could be reported in the future.

The number of parallel jobs is the maximum number of logical CPUs
offered by the host as is the default for `semgrep`.

Manual operation
--

Read and use the Makefile or call `./run-benchmarks` directly.
The bare `make` command will use the local `semgrep` command and
overall is safe to use.
```
$ make
```

This will *not upload* the results to the dashboard, as it is reserved
for CI jobs which run more or less in a consistent environment.
