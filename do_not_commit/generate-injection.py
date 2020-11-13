#!/usr/bin/env python3

import logging
import os
import subprocess
import sys
import yaml

from jinja2 import Template

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
    parser.add_argument("template_file", type=str, help="The template file to use.")
    parser.add_argument("config_file", type=str)

    args = parser.parse_args()

    with open(args.config_file, 'r') as fin:
        config = yaml.load(fin)

    with open(args.template_file, "r") as fin:
        template_file_contents = fin.read()
    t = Template(template_file_contents)
    print(t.render(config)+"\n")