# type: ignore
import os
import platform
import shutil
import stat
import sys

import setuptools

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SOURCE_DIR)
BIN_DIR = "bin"
PACKAGE_BIN_DIR = os.path.join(SOURCE_DIR, "src", "semgrep", BIN_DIR)
SEMGREP_CORE_BIN = "semgrep-core"
SEMGREP_CORE_BIN_ENV = "SEMGREP_CORE_BIN"
SEMGREP_SKIP_BIN = "SEMGREP_SKIP_BIN" in os.environ
SEMGREP_FORCE_INSTALL = "SEMGREP_FORCE_INSTALL" in os.environ
IS_WINDOWS = platform.system() == "Windows"
WHEEL_CMD = "bdist_wheel"


if WHEEL_CMD in sys.argv:
    try:
        from wheel.bdist_wheel import bdist_wheel
    except ImportError:
        raise Exception(f"The 'wheel' package is required when running '{WHEEL_CMD}'")

    class BdistWheel(bdist_wheel):
        def finalize_options(self):
            bdist_wheel.finalize_options(self)
            self.root_is_pure = False  # We have platform specific binaries

        def get_tag(self):
            _, _, plat = bdist_wheel.get_tag(self)
            python = "cp37.cp38.cp39.py37.py38.py39"
            abi = "none"
            if "macosx" in plat:
                plat = "macosx_11_0_arm64" if "arm" in plat else "macosx_10_14_x86_64"
            else:
                plat = "any"
            return python, abi, plat

    cmdclass = {WHEEL_CMD: BdistWheel}
else:
    cmdclass = {}

if IS_WINDOWS and not SEMGREP_FORCE_INSTALL:
    raise Exception(
        "Semgrep does not support Windows yet, please try again with WSL "
        "or visit the following for more information: "
        "https://github.com/returntocorp/semgrep/issues/1330"
    )

try:
    with open(os.path.join(REPO_ROOT, "README.md")) as f:
        long_description = f.read()
except FileNotFoundError:
    long_description = "**SETUP: README NOT FOUND**"


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


#
# The default behavior is to copy the semgrep-core binary
# into some other folder known to the semgrep wrapper. If somebody knows why,
# please explain why we do this.
#
# It makes testing of semgrep-core error-prone since recompiling
# semgrep-core won't perform this copy. If we can't get rid of this, can
# we use a symlink instead?
#
# The environment variable SEMGREP_SKIP_BIN bypasses this copy. What is it for?
#
if not SEMGREP_SKIP_BIN:
    binaries = [
        (SEMGREP_CORE_BIN_ENV, SEMGREP_CORE_BIN),
    ]

    for binary_env, binary_name in binaries:
        src = find_executable(binary_env, binary_name)
        dst = os.path.join(PACKAGE_BIN_DIR, binary_name)
        # The semgrep-core executable doesn't have the write
        # permission (because of something dune does?), and copyfile
        # doesn't remove the destination file if it already exists
        # but tries to truncate it, resulting in an error.
        # So we remove the destination file first if it exists.
        try:
            os.remove(dst)
        except OSError:
            pass
        shutil.copyfile(src, dst)
        os.chmod(dst, os.stat(dst).st_mode | stat.S_IEXEC)

install_requires = [
    # versions must be manually synced:
    # - cli/setup.py lists dependencies
    # - cli/Pipfile lists type hint packages for dev env
    # - .pre-commit-config.yaml's mypy hooks also list type hint packages
    #
    # These version are flexible so semgrep can coexist with other tools.
    # Flexibility is achieved by, in order of preference:
    # 1. x.0~= operator pinning to x major version
    # 2. >=x,<y operator pinning to multiple major versions
    "attrs~=21.3",
    "boltons~=21.0",
    "colorama~=0.4.0",
    "click~=8.1",
    "click-option-group~=0.5",
    "glom~=22.1",
    "requests~=2.22",
    "ruamel.yaml>=0.16.0,<0.18",
    "tqdm~=4.46",
    "packaging~=21.0",
    "jsonschema~=4.6",
    "wcmatch~=8.3",
    "peewee~=3.14",
    "defusedxml~=0.7.1",
    "urllib3~=1.26",
    "typing-extensions~=4.2",
    "python-lsp-jsonrpc~=1.0.0",
]

setuptools.setup(
    name="semgrep",
    version="0.105.0",
    author="Return To Corporation",
    author_email="support@r2c.dev",
    description="Lightweight static analysis for many languages. Find bug variants with patterns that look like source code.",
    cmdclass=cmdclass,
    install_requires=install_requires,
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/returntocorp/semgrep",
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(where="src"),
    package_dir={"": "src"},
    package_data={"semgrep": [os.path.join(BIN_DIR, "*")]},
    include_package_data=True,
    classifiers=[
        "Environment :: Console",
        "License :: OSI Approved :: GNU Lesser General Public License v2 (LGPLv2)",
        "Operating System :: MacOS",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Topic :: Security",
        "Topic :: Software Development :: Quality Assurance",
    ],
    python_requires=">=3.7",
    zip_safe=False,
)
