# type: ignore
# Used for pre-commit since it expects a setup.py in repo root
# for actual setup.py see semgrep/setup.py
from setuptools import setup

setup(
    name="semgrep_pre_commit_package",
    version="0.0.0",
    install_requires=["semgrep==0.56.0"],
)
