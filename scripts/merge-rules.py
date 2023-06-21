#!/usr/bin/env python3
# Helper script that will create a single yml file by merging all yaml/yml files
# in a directory and all subdirectories. Skips directories and files with
# a leading .
# Useful for local work (slicing and dicing rules) and as help for creation of
# .semgrep.yml files for CI
import os
import sys

from ruamel.yaml import YAML


def getRulefilesFromPath(path):
    rulefiles = []
    for root, dirs, files in os.walk(path):
        dirs[:] = [d for d in dirs if not d.startswith(".")]
        for name in files:
            file = os.path.join(root, name)
            if (file.endswith(".yml") or file.endswith(".yaml")) and (
                not name.startswith(".")
            ):
                rulefiles.append(file)
    print(f"Found {len(rulefiles)} rulefiles")
    return rulefiles


def mergeRules(rulefilelist, outputfile):
    yaml = YAML()
    rulefile = {"rules": []}
    for file in rulefilelist:
        rulefileyaml = yaml.load(open(file))
        rulefile["rules"] += rulefileyaml["rules"]
    rulecount = len(rulefile["rules"])
    print(f"Created {rulecount} rules")
    output = open(outputfile, "w")
    yaml.dump(rulefile, output)
    output.close()


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Merges all yaml/yml files in a directory and all subdirs")
        print("Skips directories and files prefixed with a .")
        print(f"Usage {sys.argv[0]} [input folder] [output filename]")
    else:
        rules = getRulefilesFromPath(sys.argv[1])
        print("Merging...")
        mergeRules(rules, sys.argv[2])
