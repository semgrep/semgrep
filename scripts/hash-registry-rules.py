#!/usr/bin/env python3
import json
import logging
import sys

from semgrep.config_resolver import Config

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
    parser.add_argument("--config", "-f")
    parser.add_argument("--verbose", "-v", action="store_true")

    args = parser.parse_args()
    if args.verbose:
        logger.setLevel(logging.DEBUG)

    logger.info(f"Fetching '{args.config}'")
    config = Config.from_config_list([args.config], None)[0]
    rules = config.get_rules(True)
    print(json.dumps({r.id: r.full_hash for r in rules}))
