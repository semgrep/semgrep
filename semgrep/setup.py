# type: ignore
import distutils.util
import os
import sys

import setuptools
from setuptools import setup
from setuptools.command.install import install
from wheel.bdist_wheel import bdist_wheel as _bdist_wheel

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
        if not sys.platform.startswith("win"):
            return _bdist_wheel.get_tag(self)

        python, abi, plat = _bdist_wheel.get_tag(self)
        python, abi = "py2.py3", "none"
        return python, abi, plat


with open("../README.md") as f:
    long_description = f.read()


class PostInstallCommand(install):
    """Post-installation for installation mode."""

    def run(self):
        # So ths builds the executable, and even installs it
        # but we can't install to the bin directory:
        #     https://github.com/pypa/setuptools/issues/210#issuecomment-216657975
        # take the advice from that comment, and move over after install
        source_dir = os.path.dirname(os.path.abspath(__file__))

        # cross_compile_target = os.getenv("PYSPY_CROSS_COMPILE_TARGET")
        # if cross_compile_target:
        #    compile_args = " --target=%s" % cross_compile_target
        #    build_dir = os.path.join(source_dir, "target", cross_compile_target, "release")
        # else:
        #    compile_args = ""
        #    build_dir = os.path.join(source_dir, "target", "release")
        if os.environ.get("PRECOMILED_LOCATION"):
            source = os.environ["PRECOMPILED_LOCATION"]
        else:
            repo_root = os.path.dirname(source_dir)
            if "osx" in distutils.util.get_platform():
                os.system(os.path.join(repo_root, "release-scripts/osx-release.sh"))
                source = "artifacts/semgrep-core"
            else:
                raise Exception("Unsupported platform")

        ## setuptools_rust doesn't seem to let me specify a musl cross compilation target
        ## so instead just build ourselves here =(.
        # if os.system("cargo build --release %s" % compile_args):
        #    raise ValueError("Failed to compile!")

        ## run this after trying to build with cargo (as otherwise this leaves
        ## venv in a bad state: https://github.com/benfred/py-spy/issues/69)
        install.run(self)

        ## we're going to install the py-spy executable into the scripts directory
        ## but first make sure the scripts directory exists
        if not os.path.isdir(self.install_scripts):
            os.makedirs(self.install_scripts)

        target = os.path.join(self.install_scripts, "semgrep-core")
        if os.path.isfile(target):
            os.remove(target)

        self.copy_file(source, target)


setup(
    name="semgrep",  # Replace with your own username
    version="0.6.0",
    author="Russell & Return 2 Corp",
    author_email="author@example.com",
    description="semgrep python wrapper",
    cmdclass={"install": PostInstallCommand, "bdist_wheel": bdist_wheel},
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/pypa/sampleproject",
    install_requires=["colorama==0.4.3", "pyyaml==5.3", "requests==2.22.0"],
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.6",
    zip_safe=False,
)
