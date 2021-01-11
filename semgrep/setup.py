# type: ignore
import distutils.util
import os
import shutil
import stat

import setuptools
from wheel.bdist_wheel import bdist_wheel

from semgrep import __VERSION__


SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SOURCE_DIR)
BIN_DIR = "bin"
PACKAGE_BIN_DIR = os.path.join(SOURCE_DIR, "semgrep", BIN_DIR)
SEMGREP_CORE_BIN = "semgrep-core"
SEMGREP_CORE_BIN_ENV = "SEMGREP_CORE_BIN"
SPACEGREP_BIN = "spacegrep"
SPACEGREP_BIN_ENV = "SPACEGREP_BIN"
SEMGREP_SKIP_BIN = "SEMGREP_SKIP_BIN" in os.environ

try:
    with open(os.path.join(REPO_ROOT, "README.md")) as f:
        long_description = f.read()
except FileNotFoundError:
    long_description = "**SETUP: README NOT FOUND**"


# TODO: what is the minimum OSX version?
MIN_OSX_VERSION = "10_14"


# from https://stackoverflow.com/questions/45150304/how-to-force-a-python-wheel-to-be-platform-specific-when-building-it # noqa
class BdistWheel(bdist_wheel):
    def finalize_options(self):
        bdist_wheel.finalize_options(self)
        # Mark us as not a pure python package (we have platform specific rust code)
        self.root_is_pure = False

    def get_tag(self):
        # this set's us up to build generic wheels.
        # note: we're only doing this for windows right now (causes packaging issues
        # with osx)
        _, _, plat = bdist_wheel.get_tag(self)
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


def find_executable(env_name, exec_name):
    # First, check for an environment override
    env_value = os.getenv(env_name)
    if env_value:
        return env_value

    # Second, fallback to any system executable
    which_name = shutil.which(exec_name)
    if which_name is not None:
        return which_name

    raise Exception(
        f"Could not find '{exec_name}' executable, tried '{env_name}' and system '{exec_name}'"
    )


if not SEMGREP_SKIP_BIN:
    binaries = [
        (SEMGREP_CORE_BIN_ENV, SEMGREP_CORE_BIN),
        (SPACEGREP_BIN_ENV, SPACEGREP_BIN),
    ]

    for binary_env, binary_name in binaries:
        src = find_executable(binary_env, binary_name)
        dst = os.path.join(PACKAGE_BIN_DIR, binary_name)
        shutil.copyfile(src, dst)
        os.chmod(dst, os.stat(dst).st_mode | stat.S_IEXEC)

setuptools.setup(
    name="semgrep",
    version=__VERSION__,
    author="Return To Corporation",
    author_email="support@r2c.dev",
    description="Lightweight static analysis for many languages. Find bug variants with patterns that look like source code.",
    cmdclass={"bdist_wheel": BdistWheel},
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/returntocorp/semgrep",
    install_requires=[
        "attrs>=19.3.0",
        "colorama>=0.4.3",
        "junit_xml==1.9",
        "requests>=2.22.0",
        # Pin exact version of 'ruamel.yaml' because of unstable API.
        "ruamel.yaml==0.16.10",
        "tqdm>=4.46.1",
        "packaging>=20.4",
        "jsonschema~=3.2.0",
        # Include 'setuptools' for 'pkg_resources' usage. We shouldn't be
        # overly prescriptive and pin the version for two reasons: 1) because
        # it may interfere with other 'setuptools' installs on the system,
        # and 2) our 'pkg_resources' API usage appears to have been available
        # in 'setuptools' for a very long time, so we don't need a recent
        # version.
        "setuptools",
    ],
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(),
    package_data={"semgrep": [os.path.join(BIN_DIR, "*")]},
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
