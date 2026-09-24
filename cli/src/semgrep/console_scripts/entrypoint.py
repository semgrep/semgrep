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

IS_WINDOWS = platform.system() == "Windows"

# The names a console script for pysemgrep can go by. setuptools writes a .exe
# launcher on Windows; everywhere else the script keeps its bare name.
PYSEMGREP_SCRIPT_NAMES = (
    ("pysemgrep.exe", "pysemgrep") if IS_WINDOWS else ("pysemgrep",)
)


def provides_pysemgrep(directory):
    """Whether a PATH lookup of "pysemgrep" would resolve inside directory."""
    for name in PYSEMGREP_SCRIPT_NAMES:
        candidate = os.path.join(directory, name)
        if os.path.isfile(candidate) and (IS_WINDOWS or os.access(candidate, os.X_OK)):
            return True
    return False


def is_same_directory(left, right):
    try:
        return os.path.samefile(left, right)
    except OSError:
        # one of them does not exist, or cannot be stat'd
        return os.path.normcase(os.path.abspath(left)) == os.path.normcase(
            os.path.abspath(right)
        )


def path_with_scripts_dir(path, scripts_dir):
    """PATH with scripts_dir added, placed so that a lookup of "pysemgrep" finds our own.

    osemgrep dispatches back to pysemgrep by name -- execvp in
    src/osemgrep/core/Pysemgrep.ml -- so the helper has to be reachable through
    PATH even when semgrep was invoked as /path/to/somewhere/bin/semgrep and is
    not on PATH at all. Adding this installation's scripts directory is what
    makes that work.

    Appending it is not enough on its own: an unrelated Semgrep earlier in PATH
    is found first, so naming one install by absolute path runs another
    install's pysemgrep, and `semgrep --version` reports the other version.

    Prepending unconditionally would fix that, but the scripts directory is a
    shared bin directory for a Homebrew, system-Python or Docker install, and
    moving it to the front reorders lookups for everything else the process
    shells out to -- semgrep runs `git` by name throughout `semgrep ci`. So the
    directory only moves ahead of a *conflicting* pysemgrep, and is appended,
    as before, when there is none.
    """
    entries = [entry for entry in path.split(os.pathsep) if entry]
    appended = os.pathsep.join([*entries, scripts_dir])

    if not provides_pysemgrep(scripts_dir):
        # nothing of ours to protect, so leave the ordering alone
        return appended

    for i, entry in enumerate(entries):
        if not provides_pysemgrep(entry):
            continue
        # the first entry that answers the lookup decides it: ours already wins
        # if that entry is our own directory, otherwise it has to go in front
        if is_same_directory(entry, scripts_dir):
            return appended
        return os.pathsep.join([*entries[:i], scripts_dir, *entries[i:]])

    return appended


# nosem: no-env-vars-on-top-level
PATH = os.environ.get("PATH", "")
# nosem: no-env-vars-on-top-level
os.environ["PATH"] = path_with_scripts_dir(PATH, sysconfig.get_path("scripts"))

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
        super().__init__(value)

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
    # cmdliner 2.x emits ANSI-styled error/help output whenever TERM is set,
    # with no isatty check (a regression from 1.x, which was always plain).
    # When our own stderr isn't a TTY (piped to a log file, captured by a
    # test harness, etc.), set NO_COLOR so cmdliner reverts to plain output.
    # Skip when the caller has an explicit color preference so force-color
    # tests and users who opt in/out stay in control.
    if (
        not sys.stderr.isatty()
        and not os.environ.get("NO_COLOR")
        and not os.environ.get("FORCE_COLOR")
        and not os.environ.get("SEMGREP_FORCE_COLOR")
    ):
        os.environ["NO_COLOR"] = "1"
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
                print(
                    "Attempting to auto-install semgrep-core-proprietary and retry the command...",
                    file=sys.stderr,
                )

                # Lazy import to keep the entry point lightweight
                from semgrep.app import auth
                from semgrep.cli import cli

                # If not logged in, prompt user to log in
                if not auth.is_logged_in_weak():
                    print(
                        "Run `semgrep login` before running `semgrep scan --pro`. Or in non-interactive environments, ensure your SEMGREP_APP_TOKEN variable is set correctly.",
                        file=sys.stderr,
                    )
                    sys.exit(2)

                # We already attempted to auto-install in this process tree and so we fail
                if os.environ.get("_SEMGREP_PRO_AUTO_INSTALLED"):
                    print(
                        "install-semgrep-pro previously reported success but the binary still isn't reachable.",
                        file=sys.stderr,
                    )
                    sys.exit(2)

                # Otherwise, install-semgrep-pro and try again
                try:
                    cli.main(args=["install-semgrep-pro"], standalone_mode=False)
                except SystemExit as exc:
                    # If installation failed, we print the error
                    if exc.code not in (0, None):
                        raise

                    # Otherwise, retry the original command now that semgrep-pro is installed
                    # Environment variable ensures that we don't try to auto-install again if something goes wrong
                    os.environ["_SEMGREP_PRO_AUTO_INSTALLED"] = "1"
                    print(
                        "Semgrep Pro Engine installed successfully, retrying the original command...",
                        file=sys.stderr,
                    )
                    exec_osemgrep()
                    return

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
        try:
            # nosem: dangerous-subprocess-use-tainted-env-args
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
    # This is a workaround for stdio and stdout encoding issues on Windows.
    # Instead of relying on the users setting PYTHONIOENCODING=utf8 when
    # running on Windows and redirecting the stdout and stderr to files, we do
    # it in the console scripts.
    # https://docs.python.org/3/library/sys.html#sys.stdout
    if IS_WINDOWS and not sys.stdout.isatty():
        sys.stdout.reconfigure(encoding="utf-8")
        sys.stderr.reconfigure(encoding="utf-8")

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
