# type: ignore
import contextlib
import distutils.util
import os

import setuptools
from setuptools import setup
from setuptools.command.install import install
from wheel.bdist_wheel import bdist_wheel as _bdist_wheel

from semgrep import __VERSION__


@contextlib.contextmanager
def chdir(dirname=None):
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)


# TODO: what is the minimum OSX version?
MIN_OSX_VERSION = "10_14"

# from https://stackoverflow.com/questions/45150304/how-to-force-a-python-wheel-to-be-platform-specific-when-building-it # noqa
class bdist_wheel(_bdist_wheel):
    def finalize_options(self):
        _bdist_wheel.finalize_options(self)
        # Mark us as not a pure python package (we have platform specific rust code)
        self.root_is_pure = False

    def get_tag(self):
        # this set's us up to build generic wheels.
        # note: we're only doing this for windows right now (causes packaging issues
        # with osx)
        _, _, plat = _bdist_wheel.get_tag(self)
        # to debug "ERROR: *.whl is not a supported wheel on this platform.":
        # from setuptools.pep425tags import get_supported
        # get_supported()
        python = ".".join(["cp36", "cp37", "cp38", "py36", "py37", "py38"])
        abi = "none"

        if "macosx" in plat:
            plat = f"macosx_{MIN_OSX_VERSION}_x86_64"

        # The binary we build is statically linked & manylinux compatible and alpine compatible
        # there is no way to specify works with alpine on pypi so set platform as any.
        # Note that semgrep-core is still incompatible with Windows
        if plat == "linux_x86_64":
            plat = "any"
        elif plat == "linux_i686":
            plat = "manylinux1_i686"

        return python, abi, plat


try:
    with open("../README.md") as f:
        long_description = f.read()
except FileNotFoundError:
    long_description = ""

source_dir = os.path.dirname(os.path.abspath(__file__))
repo_root = os.path.dirname(source_dir)

# Lifted with love (and edits) from https://github.com/benfred/py-spy/blob/master/setup.py
class PostInstallCommand(install):
    """Post-installation for installation mode."""

    def copy_binary(self, exec_name):
        if "TOX_ENV_NAME" in os.environ:
            print("Not attempting to install binary while running under tox")
            return
        if "HOMEBREW_SYSTEM" in os.environ:
            print("Not attempting to install binary while running under homebrew")
            install.run(self)
            return
        # So ths builds the executable, and even installs it
        # but we can't install to the bin directory:
        #     https://github.com/pypa/setuptools/issues/210#issuecomment-216657975
        # take the advice from that comment, and move over after install

        if "osx" in distutils.util.get_platform():
            with chdir(repo_root):
                os.system(os.path.join(repo_root, "scripts", "osx-release.sh"))
                source = os.path.join(repo_root, "artifacts", exec_name)
        else:
            with chdir(repo_root):
                os.system(os.path.join(repo_root, "scripts", "ubuntu-release.sh"))
                source = os.path.join(repo_root, "semgrep-files", exec_name)

        ## run this after trying to build (as otherwise this leaves
        ## venv in a bad state: https://github.com/benfred/py-spy/issues/69)
        install.run(self)

        ## we're going to install the executable (binary) into the scripts directory
        ## but first make sure the scripts directory exists
        if not os.path.isdir(self.install_scripts):
            os.makedirs(self.install_scripts)

        target = os.path.join(self.install_scripts, exec_name)
        if os.path.isfile(target):
            os.remove(target)

        self.copy_file(source, target)

    def run(self):
        self.copy_binary("semgrep-core")
        self.copy_binary("spacegrep")
        # FIXME: with an extra 3 MB binary we exceed the 100 MB limit on PyPI
        # Suggested fixes:
        # - spacecat and spacegrep are now the same executable. We only need
        #   the command (= executable file name) to be 'spacecat' to trigger
        #   the spacecat behavior.
        #   This can be achieved with a symlink or by copying 'spacegrep'
        #   to 'spacecat' at installation time. Spacecat is a utility for
        #   printing an AST, which is nice to have but not called
        #   by semgrep or semgrep-core.
        # - Use 'strip' to reduce the size of the semgrep-core executable.
        #   Its big size is due to large parse tables that support
        #   the LR(1) parsing algorithm used by menhir and tree-sitter.
        #   For tree-sitter, those are the 'parser.c' files generated for each
        #   grammar. We'll have more and more of such files, so the size of
        #   our static semgrep-core executable is expected to keep growing.
        #
        # Original code:
        #
        # self.copy_binary("spacecat")


setup(
    name="semgrep",
    version=__VERSION__,
    author="Return To Corporation",
    author_email="support@r2c.dev",
    description="Lightweight static analysis for many languages. Find bug variants with patterns that look like source code.",
    cmdclass={"install": PostInstallCommand, "bdist_wheel": bdist_wheel},
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/returntocorp/semgrep",
    install_requires=[
        "attrs>=19.3.0",
        "colorama>=0.4.3",
        "junit_xml==1.9",
        "requests>=2.22.0",
        # exact version because of unstable API
        "ruamel.yaml==0.16.10",
        "tqdm>=4.46.1",
        "packaging>=20.4",
        "jsonschema~=3.2.0",
    ],
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(),
    include_package_data=True,
    classifiers=[
        "Environment :: Console",
        "License :: OSI Approved :: GNU Lesser General Public License v2 (LGPLv2)",
        "Operating System :: MacOS",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Topic :: Security",
        "Topic :: Software Development :: Quality Assurance",
    ],
    python_requires=">=3.6",
    zip_safe=False,
)
