import logging
import os
import subprocess
import sys
import venv
from pathlib import Path

logger = logging.getLogger(__file__)
logger.setLevel(logging.INFO)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    # Add arguments here
    parser.add_argument("--against", "-a")
    parser.add_argument("--semgrep-core", "-s")

    args = parser.parse_args()

    builder = venv.EnvBuilder(
        clear=True,
        with_pip=True,
    )
    venv_path = Path(f"semgrep-{args.against}")
    builder.create(venv_path)

    # cf. https://docs.python.org/3/library/venv.html
    # When a virtual environment is active, the VIRTUAL_ENV environment variable is
    # set to the path of the virtual environment. This can be used to check if one
    # is running inside a virtual environment.
    #
    # You don’t specifically need to activate an environment; activation just prepends
    # the virtual environment’s binary directory to your path, so that “python” invokes
    # the virtual environment’s Python interpreter and you can run installed scripts
    # without having to use their full path. However, all scripts installed in a virtual
    # environment should be runnable without activating it, and run with the virtual
    # environment’s Python automatically.

    os.environ["VIRTUAL_ENV"] = str(venv_path.absolute())
    os.environ[
        "PATH"
    ] = f"{str(venv_path.absolute() / 'bin')}{os.pathsep}{os.environ['PATH']}"

    subprocess.run(["pip", "install", f"semgrep=={args.against}"])

    subprocess.run(
        [
            "which",
            "semgrep",
        ]
    )

    subprocess.run(["semgrep", "--version"])
