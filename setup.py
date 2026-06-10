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
# Used for pre-commit since it expects a setup.py in repo root
# for actual setup.py see cli/setup.py
from setuptools import setup

setup(
    name="semgrep_pre_commit_package",
    version="1.165.0",
    install_requires=["semgrep==1.165.0"],
    packages=[],
)
