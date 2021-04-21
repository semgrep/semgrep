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

Troubleshooting CI with the semgrep-dev Docker image
--

_CI uses CircleCI or GitHub Actions, configured in the standard places
(`.circleci`, `.github/workflows`). See those files to determine which
jobs run and when._

We maintain a Docker build that comes with `semgrep`
pre-installed. It is meant for daily benchmarks and other
jobs that use the development version of semgrep. The image URL is
[`returntocorp/semgrep-dev:develop`](https://hub.docker.com/r/returntocorp/semgrep-dev/tags).
It is built and pushed to DockerHub by a CI job that triggers each
time there's a change on the main branch of the `semgrep` repo.

```
$ docker pull returntocorp/semgrep-dev:develop     # updates your local copy
$ docker run -it returntocorp/semgrep-dev:develop  # starts bash in container
```

If you want to test some of your local code inside the container, you
would mount your folder using the `-v` option. The usage is
`-v SRC:DST` where SRC is an absolute path to your original folder and DST is
the absolute path you want it to have in the container. The home
folder for this image is set to `/home/semgrep`. For example you'd do
this:

```
$ ls
my_stuff
$ docker run -v "$(pwd)"/my_stuff:/home/semgrep/my_stuff -it returntocorp/semgrep-dev:develop
bash-5.1$ whoami
semgrep
bash-5.1$ ls ~
my_stuff
bash-5.1$ semgrep --version
0.46.0
```
