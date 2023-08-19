# Semgrep benchmarks

This folder is for running realistic benchmarks for semgrep, as
opposed to more focused tests.

The main results can be visualized over time here:
https://metabase.corp.r2c.dev/question/560-semgrep-bench-all-history
(this is accessible only to Semgrep employees).

## Requirements

The `semgrep` command must be available, as well as generic development
tools including git and python3.

If you want to use the --plot-benchmarks option, you will need to install the
tk package, as well as pip install the matplotlib and pandas Python libraries.

## Architecture

Each benchmark has a name. For each benchmark, we run the standard
semgrep commands as well as variants which disable or enable certain
optimizations.

The workspace looks like this:

```
.
├── bench
│   ├── dummy
│   │   ├── input
│   │   │   ├── dummy
│   │   │   ├── rules
│   │   │       └── exec.yaml
│   │   │   ├── targets
│   │   │           ├── hello.js
│   │   │           └── malformed.js
│   │   └── prep
│   └── njs
│       ├── input
│       │   ├── juice-shop/ (lots of files)
│       │   └── njsscan/ (lots of files)
│       └── prep
├── rules
|   └── semgrep_fast.yml
├── configs
|   └── ci_medium_repos.yaml
|
├── Makefile
├── README.md
└── run-benchmarks
```

The total duration of each benchmark is uploaded to the [semgrep
dashboard](https://dashboard.semgrep.dev/metrics), for example as
`semgrep.bench.njs.std.total-time`. Other
metrics such as memory usage could be reported in the future.

Then you can use some SQL queries on metabase to slice and dice
and visualize those metrics (e.g., https://metabase.corp.r2c.dev/question/560-semgrep-bench-all-history )

The number of parallel jobs is the maximum number of logical CPUs
offered by the host as is the default for `semgrep`.

## Manual operation

Read and use the Makefile or call `./run-benchmarks` directly.
The bare `make` command will use the local `semgrep` command and
overall is safe to use.

```
$ make
```

This will _not upload_ the results to the dashboard, as it is reserved
for CI jobs which run more or less in a consistent environment.

## Reproducing the CI benchmarks

To reproduce the benchmarks as run in CI, see the toplevel Makefile.
(Updates to this header should be reflected in the comment in that file.)

The benchmarks run in CI run three scripts (see `scripts/run-benchmarks.sh`):

1. `perf/run-benchmarks` -- runs with a specific version of Semgrep,
   installed via pip, and then the local version. At the end, the version
   of semgrep installed will be the local version
2. `perf/compare-perf` -- this checks how much the benchmarks deviate by
   and posts an update if it increases by a lot or a little
3. `perf/compare-bench-findings` -- this checks the findings to confirm
   that they are still the same as the expected snapshots

For local debugging, it is often easiest to run just the `perf/run-benchmarks`
command in `scripts/run-benchmarks.sh`.

TODO: Running the script may modify `cli/Pipfile` and `cli/Pipfile.lock`. Those
changes should not be committed

## Modifying what benchmarks are run

The benchmarks to run are controlled by the configs, which reside in `perf/configs`.
Below is an example config:

```
runs:
  - name: zulip # zulip rules on zulip
    repos:
      - url: https://github.com/zulip/zulip
        commit_hash: 829f9272d2c4299a0c0a37a09802248d8136c0a8
    rule_configs:
      - rules/zulip/semgrep.yml
    opts: [--fast]
```

The opts are the command line arguments passed to Semgrep.

In addition, it is possible to compare multiple versions of Semgrep by modifying
`SEMGREP_VARIANTS` and not including the flag `--std-only`. For instance, if you
made a version of Semgrep that ran with a matching cache (`--matching-cache`) and
one without (`--no-matching-cache`), you could run both by adding them to the
`SEMGREP_VARIANTS`. You can also include `semgrep-core` options in the same way
(see `variant.py`).

If you have multiple variants and are running locally, you can easily compare the
effect visually by adding `--plot-benchmarks`.

## Troubleshooting CI with the semgrep Docker image

_CI uses CircleCI or GitHub Actions, configured in the standard places
(`.circleci`, `.github/workflows`). See those files to determine which
jobs run and when._

We maintain a Docker build that comes with `semgrep`
pre-installed. It can also be used for daily benchmarks and other
jobs that use the development version of semgrep. The image URL is
[`returntocorp/semgrep:develop`](https://hub.docker.com/r/returntocorp/semgrep/tags).
It is built and pushed to DockerHub by a CI job that triggers each
time there's a change on the main branch of the `semgrep` repo.

```
$ docker pull returntocorp/semgrep:develop     # updates your local copy
$ docker run -it returntocorp/semgrep:develop  # starts bash in container
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
$ docker run -v "$(pwd)"/my_stuff:/home/semgrep/my_stuff -it returntocorp/semgrep:develop
bash-5.1$ whoami
semgrep
bash-5.1$ ls ~
my_stuff
bash-5.1$ semgrep --version
0.46.0
```
