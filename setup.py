# type: ignore
# Used for pre-commit since it expects a setup.py in repo root
# for actual setup.py see cli/setup.py
from setuptools import setup

setup(
    name="semgrep_pre_commit_package",
    version="1.14.0",
    install_requires=["semgrep==1.14.0"],
    packages=[],
)
def lambda_handler(event, context):
    program = "bash"
    loop = asyncio.new_event_loop()
    # ruleid: dangerous-asyncio-create-exec
    proc = loop.run_until_complete(asyncio.subprocess.create_subprocess_exec(program, [program, "-c", event['cmd']]))
def lambda_handler(event, context):
    program = "bash"
    loop = asyncio.new_event_loop()
    # ruleid: dangerous-asyncio-create-exec
    proc = loop.run_until_complete(asyncio.subprocess.create_subprocess_exec(program, [program, "-c", event['cmd']]))
