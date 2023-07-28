# type: ignore
import os
import platform
import shutil
import sys

import setuptools

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SOURCE_DIR)
# pad: is this still used? git grep SEMGREP_FORCE_INSTALL does not return anything
SEMGREP_FORCE_INSTALL = "SEMGREP_FORCE_INSTALL" in os.environ
IS_WINDOWS = platform.system() == "Windows"
# See ../scripts/build-wheels.sh, which is called from our GHA workflows.
# This script assumes the presence of a semgrep-core binary copied under
# cli/src/semgrep/bin by the caller (the GHA workflow).
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
            python = "cp37.cp38.cp39.cp310.cp311.py37.py38.py39.py310.py311"
            abi = "none"
            if plat == "linux_x86_64":
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


install_requires = [
    # versions must be manually synced:
    # - cli/setup.py lists dependencies
    # - cli/Pipfile lists type hint packages for dev env
    # - .pre-commit-config.yaml's mypy hooks also list type hint packages
    #
    # These specifiers are flexible so semgrep can coexist with other tools.
    # Even though we recommend giving semgrep its own virtualenv
    # (or using the official returntocorp/semgrep Docker image),
    # many users will first try to install it in their project's virtualenv.
    #
    # Flexibility is achieved by, in order of preference:
    # 1. >=x if you know the earliest version that works with Semgrep
    # 2. >=x,<y if you know the earliest version that works with Semgrep,
    #    and know that a later version breaks Semgrep.
    # 3. ~=x.0 if you don't know the earliest version that works with Semgrep
    #
    # Try to go from option 3 to 1 over time as you learn more about the codebase.
    "attrs>=21.3",
    "boltons~=21.0",
    "click-option-group~=0.5",
    "click~=8.1",
    "colorama~=0.4.0",
    "defusedxml~=0.7.1",
    "glom~=22.1",
    "jsonschema~=4.6",
    "packaging>=21.0",
    "peewee~=3.14",
    "python-lsp-jsonrpc~=1.0.0",
    "requests~=2.22",
    "rich>=12.6.0",
    "ruamel.yaml>=0.16.0,<0.18",
    "tomli~=2.0.1",
    "typing-extensions~=4.2",
    "urllib3~=1.26",
    "wcmatch~=8.3",
]

extras_require = {"experiments": ["jsonnet~=0.18"]}

setuptools.setup(
    name="semgrep",
    version="1.34.0",
    author="Return To Corporation",
    author_email="support@r2c.dev",
    description="Lightweight static analysis for many languages. Find bug variants with patterns that look like source code.",
    cmdclass=cmdclass,
    install_requires=install_requires,
    extras_require=extras_require,
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/returntocorp/semgrep",
    scripts=["bin/semgrep", "bin/pysemgrep"],
    packages=setuptools.find_packages(where="src"),
    package_dir={"": "src"},
    package_data={"semgrep": [os.path.join("bin", "*")]},
    include_package_data=True,
    classifiers=[
        "Environment :: Console",
        "License :: OSI Approved :: GNU Lesser General Public License v2 (LGPLv2)",
        "Operating System :: MacOS",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Security",
        "Topic :: Software Development :: Quality Assurance",
    ],
    python_requires=">=3.7",
    zip_safe=False,
)
