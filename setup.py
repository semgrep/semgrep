# type: ignore
# Used for pre-commit since it expects a setup.py in repo root
# for actual setup.py see cli/setup.py
from setuptools import setup

setup(
    name="semgrep_pre_commit_package",
    version="0.103.0",
    install_requires=["semgrep==0.103.0"],
    packages=[],
)
