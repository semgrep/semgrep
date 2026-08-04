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
import platform
import subprocess
import sys

import setuptools

IS_WINDOWS = platform.system() == "Windows"
# See ../scripts/build-wheels.sh, which is called from our GHA workflows.
# This script assumes the presence of a semgrep-core binary copied under
# cli/src/semgrep/bin by the caller (the GHA workflow).
WHEEL_CMD = "bdist_wheel"


# coupling: this function is duplicated in src/semgrep/commands/install.py.
# Deduplication would require setup.py to import from the semgrep package, which
# is not ideal
def linux_detect_libc():
    try:
        result = subprocess.run(
            ["ldd", "--version"],
            capture_output=True,
            text=True,
        )
        # musl's ldd prints to stderr, glibc's ldd to stdout
        out = result.stdout + result.stderr
        if "musl" in out:
            return "musl"
    except Exception:
        pass
    return "glibc"


# To prevent potential compatibility issues when mixing glibc and libmusl,
# PyPI does not accept the default linux_x86_64 and linux_aarch64 platform
# tags. We build semgrep on glibc and musl, so we must make sure we tag
# each build as either glibc (manylinux) or musl (musllinux) compatible
#
# NOTE: semgrep-core is dynamically linked to the user's libc; the libc version
# in these tags MUST match the libc version we use to build the binary, see:
# musllinux: https://peps.python.org/pep-0656/
# manylinux: https://peps.python.org/pep-0600/
plat_libc_to_tag = {
    ("linux_aarch64", "musl"): "musllinux_1_2_aarch64",
    ("linux_x86_64", "musl"): "musllinux_1_2_x86_64",
    ("linux_aarch64", "glibc"): "manylinux_2_34_aarch64",
    ("linux_x86_64", "glibc"): "manylinux_2_34_x86_64",
}


cmdclass = {}

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

            # we translate the default linux_<arch> platform tag to either
            # musllinux_<ver>_<arch> or manylinux_<ver>_arch depending on which
            # system we are on; see plat_libc_to_tag at the top of this file.
            if plat.startswith("linux"):
                lib = linux_detect_libc()
                plat = plat_libc_to_tag[(plat, lib)]

            # The macOS Python binary is sometimes a universal binary, which leads to a
            # platform name of "macosx_xx_x_universal2" in the wheel tag. Unfortunately,
            # our binary is not built as universal, so we must detect the architecture of
            # the actual machine this is running on and clarify that we are only building
            # for that one.
            elif plat.startswith("macos") and "universal" in plat:
                machine = platform.machine()
                if machine == "x86_64":
                    plat = "macosx_10_14_x86_64"
                elif machine == "arm64":
                    plat = "macosx_11_0_arm64"
                else:
                    raise Exception(f"Unrecognized macOS machine {machine!r}")

            return python, abi, plat

    cmdclass[WHEEL_CMD] = BdistWheel

setuptools.setup(
    cmdclass=cmdclass,
)
