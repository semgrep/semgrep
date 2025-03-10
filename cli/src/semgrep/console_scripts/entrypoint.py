#!/usr/bin/env python3
# This file is the Semgrep CLI entry point of the Semgrep pip package,
# the Semgrep HomeBrew package, and the Semgrep Docker container.
#
# In the future we may have different entry points when packaging Semgrep
# with Cargo, Npm, Opam, or even with Docker (ideally the entry point would
# be src/main/Main.ml without any wrapper around once osemgrep is finished).
#
# The main purpose of this small wrapper is to dispatch
# either to the legacy pysemgrep (see the pysemgrep script in this
# directory), or to the new osemgrep (accessible via the semgrep-core binary
# under cli/src/semgrep/bin/ or somewhere in the PATH), or even to
# osemgrep-pro (accessible via the semgrep-core-proprietary binary).
#
# It would be faster and cleaner to have a Bash script instead of a Python
# script here, but actually the overhead of Python here is just 0.015s.
# Moreover, it is sometimes hard from a Bash script to find where is installed
# semgrep-core, but it is simple from Python because you can simply use
# importlib.resources. We could also use 'pip show semgrep' from a Bash script
# to find semgrep-core, but will 'pip' be in the PATH? Should we use 'pip' or
# 'pip3'?
# Again, it is simpler to use a Python script and leverage importlib.resources.
# Another alternative would be to always have semgrep-core in the PATH,
# but when trying to put this binary in cli/bin, setuptools is yelling
# and does not know what to do with it. In the end, it is simpler to use
# a *Python* script when installed via a *Python* package manager (pip).
#
# NOTE: if you modify this file, you will need to `pipenv install --dev`
# if you want to test the change under `pipenv shell`.
import importlib.resources
import os
import platform
import shutil
import subprocess
import sys
import sysconfig
import warnings

# alt: you can also add '-W ignore::DeprecationWarning' after the python3 above,
# but setuptools and pip adjust this line when installing semgrep so we need
# to do this instead.

warnings.filterwarnings("ignore", category=DeprecationWarning)

# Add the directory containing this script in the PATH, so the pysemgrep
# script will also be in the PATH.
# Some people don't have semgrep in their PATH and call it instead
# explicitly as in /path/to/somewhere/bin/semgrep, but this means
# that calling pysemgrep from osemgrep would be difficult because
# it would not be in the PATH (we would need to pass its path to osemgrep,
# which seems more complicated).
# nosem: no-env-vars-on-top-level
PATH = os.environ.get("PATH", "")
# nosem: no-env-vars-on-top-level
os.environ["PATH"] = PATH + os.pathsep + sysconfig.get_path("scripts")

IS_WINDOWS = platform.system() == "Windows"

PRO_FLAGS = ["--pro", "--pro-languages", "--pro-intrafile"]

if IS_WINDOWS:
    # NOTE: we conditionally import colorama to avoid importing it
    # unnecessarily on other platforms in the entrypoint script, which needs to
    # be as 'light' as possible.
    from colorama.winterm import enable_vt_processing

    # On Windows, enable virtual terminal processing to correctly process ANSI
    # escape sequences for pretty, colored output. We want to enable this as
    # early as possible to correctly process ANSI escape sequences in all the
    # subcommands both in osemgrep and pysemgrep. See
    # https://learn.microsoft.com/en-us/windows/console/setconsolemode for more
    # information.
    enable_vt_processing(sys.stdout.fileno())
    enable_vt_processing(sys.stderr.fileno())


class CoreNotFound(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value


# Similar to cli/src/semgrep/engine.py check_is_correct_pro_version
def is_correct_pro_version(core_path):
    """
    We want to be careful about what we import here in order to keep this
    script lightweight. However, this file just defines a single constant and
    it takes well under a millisecond to import this.

    This can be verified with `time python -c 'import semgrep; print(semgrep.__VERSION__)'`
    """
    from semgrep import __VERSION__

    # Duplicate of cli/src/semgrep/semgrep_core.py pro_version_stamp_path
    stamp_path = core_path.parent / "pro-installed-by.txt"
    if stamp_path.is_file():
        with stamp_path.open("r") as f:
            version_at_install = f.readline().strip()
            return version_at_install == __VERSION__
    else:
        return False


# similar to cli/src/semgrep/semgrep_core.py compute_executable_path()
def find_semgrep_core_path(pro=False, extra_message=""):
    if pro:
        core = "semgrep-core-proprietary"
    else:
        core = "semgrep-core"

    if IS_WINDOWS:
        core += ".exe"

    # First, try the packaged binary.
    try:
        # the use of .path causes a DeprecationWarning hence the
        # filterwarnings above
        with importlib.resources.path("semgrep.bin", core) as path:
            if path.is_file():
                if pro and not is_correct_pro_version(path):
                    raise CoreNotFound(
                        f"The installed version of {core} is out of date.{extra_message}"
                    )
                return str(path)
    except (FileNotFoundError, ModuleNotFoundError):
        pass

    # Second, try in PATH. In certain context such as Homebrew
    # (see https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb)
    # or Docker (see ../../Dockerfile), we actually copy semgrep-core in
    # /usr/local/bin (or in a bin/ folder in the PATH). In those cases,
    # there is no /.../site-packages/semgrep-xxx/bin/semgrep-core.
    # In those cases, we want to grab semgrep-core from the PATH instead.
    path = shutil.which(core)
    if path is not None:
        return path

    raise CoreNotFound(
        f"Failed to find {core} in PATH or in the semgrep package.{extra_message}"
    )


# TODO: we should just do 'execvp("pysemgrep", sys.argv)'
# but this causes some regressions with --test (see PA-2963)
# and autocomplete (see #8359)
# TODO: we should get rid of autocomplete anyway (it's a Python Click
# thing not supported by osemgrep anyway),
# TODO: we should fix --test instead.
# The past investigation of Austin is available in #8360 PR comments
def exec_pysemgrep():
    import semgrep.main

    sys.exit(semgrep.main.main())


# We could have moved the code below in a separate 'osemgrep' file, like
# for 'pysemgrep', but we don't want users to be exposed to another command,
# so it is better to hide it.
# We expose 'pysemgrep' because osemgrep itself might need to fallback to
# pysemgrep and it's better to avoid the possibility of an infinite loop
# by simply using a different program name. Morever, in case of big problems,
# we can always tell users to run pysemgrep instead of semgrep and be sure
# they'll get the old behavior.
def exec_osemgrep():
    argv = sys.argv
    if any(pro_flag in argv for pro_flag in PRO_FLAGS):
        try:
            path = find_semgrep_core_path(
                pro=True,
                extra_message="\nYou may need to run `semgrep install-semgrep-pro`",
            )
        except CoreNotFound as e:
            print(str(e), file=sys.stderr)
            if sys.argv[1] == "ci":
                # CI users usually want things to just work. In particular, if they
                # are running `semgrep ci --pro` they don't want to have to add an
                # extra step to install-semgrep-pro. This wrapper doesn't have a way
                # to install semgrep-pro, however, so have them run legacy `semgrep`.
                print(
                    "Since `semgrep ci` was run, defaulting to legacy semgrep",
                    file=sys.stderr,
                )
                exec_pysemgrep()
            else:
                sys.exit(2)
        # If you call semgrep-core-proprietary as osemgrep-pro, then we get
        # osemgrep-pro behavior, see semgrep-proprietary/src/main/Pro_main.ml
        sys.argv[0] = "osemgrep-pro"
    else:
        try:
            path = find_semgrep_core_path()
        except CoreNotFound as e:
            print(str(e), file=sys.stderr)
            # fatal error, see src/osemgrep/core/Exit_code.ml
            sys.exit(2)

        # If you call semgrep-core as osemgrep, then we get
        # osemgrep behavior, see src/main/Main.ml
        sys.argv[0] = "osemgrep"
    if IS_WINDOWS:
        # On Windows, os.execvp spawns a background process instead of
        # replacing the current one, which breaks CLI interactivity. Therefore,
        # we use subprocess.run to spawn a process that maintains interactive
        # behavior. We stick with os.execvp on POSIX systems because it
        # correctly replaces the current process, which is the desired behavior
        # and avoids unnecessary extra processes.
        # nosem: dangerous-subprocess-use-tainted-env-args
        try:
            child = subprocess.run(sys.argv, executable=str(path))
        except KeyboardInterrupt:
            # We don't want the stack trace on user interrupt
            print(str("Aborted!"), file=sys.stderr)
            sys.exit(130)
        sys.exit(child.returncode)
    else:
        # nosem: dangerous-os-exec-tainted-env-args
        os.execvp(str(path), sys.argv)


# Needed for similar reasons as in pysemgrep, but only for the legacy
# flag to work
def main():
    # escape hatch for users to pysemgrep in case of problems (they
    # can also call directly 'pysemgrep').
    if "--legacy" in sys.argv:
        sys.argv.remove("--legacy")
        exec_pysemgrep()
    elif "--experimental" in sys.argv:
        exec_osemgrep()
    else:
        # we now default to osemgrep! but this will usually exec
        # back to pysemgrep for most commands (for now)
        # We activate the new CLI UX only when semgrep is invoked directly
        # (and legacy is not specified)
        # and osemgrep needs to fallback on pysemgrep
        os.environ["SEMGREP_NEW_CLI_UX"] = f"{int(sys.stdout.isatty())}"
        exec_osemgrep()


if __name__ == "__main__":
    main()
