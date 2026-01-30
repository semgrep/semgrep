#
# Copyright (c) 2020-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
# type: ignore
import os
import platform
import sys

import setuptools

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SOURCE_DIR)
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

            # For more information about python compatibility tags, check out:
            # https://packaging.python.org/en/latest/specifications/platform-compatibility-tags/

            # We support Python 3.10+
            # coupling: if you drop support for some python, you'll probably
            # have to update 'python_requires' at the end of this file
            # and a few workflows as show for example in this PR:
            # https://github.com/semgrep/semgrep-proprietary/pull/2606/files
            # coupling: semgrep.libsonnet default_python_version
            python = "cp310.cp311.cp312.cp313.cp314.py310.py311.py312.py313.py314"

            # We don't require a specific Python ABI
            abi = "none"

            # To prevent potential compatibility issues when mixing glibc and libmusl,
            # PyPI does not accept the default linux_x86_64 and linux_aarch64 platform
            # tags. Instead, package maintainers must explicitly identify if their package
            # supports glibc and/or libmusl. Semgrep-core is statically compiled,
            # so this isn't a concern for us.
            if plat == "linux_aarch64":
                plat = "musllinux_1_0_aarch64.manylinux2014_aarch64"
            elif plat == "linux_x86_64":
                plat = "musllinux_1_0_x86_64.manylinux2014_x86_64"
            # The macOS Python binary is sometimes a universal binary, which leads to a
            # platform name of "macosx_10_9_universal2" in the wheel tag. Unfortunately,
            # our binary is not built as universal, so we must detect the architecture of
            # the actual machine this is running on and clarify that we are only building
            # for that one.
            elif plat == "macosx_10_9_universal2":
                machine = platform.machine()
                if machine == "x86_64":
                    plat = "macosx_10_14_x86_64"
                elif machine == "arm64":
                    plat = "macosx_11_0_arm64"
                else:
                    raise Exception(f"Unrecognized macOS machine {machine!r}")

            return python, abi, plat

    cmdclass = {WHEEL_CMD: BdistWheel}
else:
    cmdclass = {}

# setting readme logic, taken out in pull/5420 but brought back temporarily
try:
    with open(os.path.join(REPO_ROOT, "README.md")) as f:
        long_description = f.read()
except FileNotFoundError:
    long_description = "**SETUP: README NOT FOUND**"

setuptools.setup(
    long_description=long_description,
    long_description_content_type="text/markdown",
    cmdclass=cmdclass,
)
