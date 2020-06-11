# type: ignore
import contextlib
import distutils.util
import os

import setuptools
from setuptools import setup
from setuptools.command.install import install
from wheel.bdist_wheel import bdist_wheel as _bdist_wheel


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
            plat = f"macosx_{MIN_OSX_VERSION}_x86_64.whl"

        # The binary we build is statically linked & manylinux compatible, so change platform
        # accordingly
        if plat == "linux_x86_64":
            plat = "manylinux1_x86_64"
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

    def run(self):
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

        if os.environ.get("PRECOMPILED_LOCATION"):
            source = os.environ["PRECOMPILED_LOCATION"]
        else:
            os.putenv("SKIP_NUITKA", "TRUE")
            if "osx" in distutils.util.get_platform():
                with chdir(repo_root):
                    os.system(os.path.join(repo_root, "release-scripts/osx-release.sh"))
                    source = os.path.join(repo_root, "artifacts/semgrep-core")
            else:
                with chdir(repo_root):
                    os.system(
                        os.path.join(repo_root, "release-scripts/ubuntu-release.sh")
                    )
                    source = os.path.join(repo_root, "semgrep-files/semgrep-core")

        ## run this after trying to build (as otherwise this leaves
        ## venv in a bad state: https://github.com/benfred/py-spy/issues/69)
        install.run(self)

        ## we're going to install the semgrep-core executable into the scripts directory
        ## but first make sure the scripts directory exists
        if not os.path.isdir(self.install_scripts):
            os.makedirs(self.install_scripts)

        target = os.path.join(self.install_scripts, "semgrep-core")
        if os.path.isfile(target):
            os.remove(target)

        self.copy_file(source, target)


setup(
    name="semgrep",
    version="0.10.1",
    author="Return To Corporation",
    author_email="support@r2c.dev",
    description="Fast and syntax-aware semantic code pattern search for many languages: like grep but for code",
    cmdclass={"install": PostInstallCommand, "bdist_wheel": bdist_wheel},
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/returntocorp/semgrep",
    install_requires=[
        "colorama>=0.4.3",
        # exact version because of unstable API
        "ruamel.yaml==0.16.10",
        "requests>=2.22.0",
        "attrs>=19.3.0",
    ],
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU Lesser General Public License v2 (LGPLv2)",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.6",
    zip_safe=False,
)
