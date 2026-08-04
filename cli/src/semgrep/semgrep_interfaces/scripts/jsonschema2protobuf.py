#!/usr/bin/env python3
#
# Chris Dolan (chris@semgrep.com)
# Copyright (C) Semgrep 2022
#
# This script translates from jsonschema to protobuf v3 definitions.
# Stable field numbers are calculated using Java's String.hashCode() algorithm
#
# Note: This is not a rigorous 1:1 mapping from jsonschema to protobuf; there
# are likely bugs and/or untranslatable structures.
#

import os
import hashlib
import json
import logging
import sys

logger = logging.getLogger(__name__)

MAX_FIELD_NUMBER = 536870911  # 2^29-1
FIRST_RESERVED_FIELD_NUMBER = 19000
LAST_RESERVED_NUMBER = 19999

JSON_SCHEMA_TO_MESSAGE_TYPE = {
    "object": "message",
    "string": "string",
    "integer": "int64",
    "boolean": "bool",
    "number": "float",
}

JSON_SCHEMA_SCALAR_TYPES = set(["string", "integer", "boolean", "number"])


def hash_code(s):
    return sum([ord(c) * pow(31, i) for i, c in enumerate(s)])


def field_num(s):
    iterations = 0
    num = None
    while not num or (
        num >= FIRST_RESERVED_FIELD_NUMBER and num <= LAST_RESERVED_NUMBER
    ):
        num = hash_code(s + " " * iterations) % MAX_FIELD_NUMBER
        iterations += 1
    return num


def pascal_case(s):
    return s.replace("_", " ").title().replace(" ", "")


def get_type(item):
    t = item.get("type")
    if isinstance(t, str):
        return t
    elif isinstance(t, list):
        t.remove("null")
        if len(t) == 1:
            return t[0]
    else:
        return None


class Message:
    def __init__(self, name, fields):
        self.name = name
        self.fields = fields

    def __str__(self):
        return "message {name} {{\n{fields}\n}}\n".format(
            name=self.name, fields="\n".join([str(f) for f in self.fields])
        )


class Field:
    def __init__(self, name, ftype, rule=None):
        self.name = name
        self.ftype = ftype
        self.rule = rule

    def __str__(self):
        return f"  {self.rule if self.rule else ''}{' ' if self.rule else ''}{self.ftype} {self.name} = {field_num(self.name)};"


class Visitor:
    def __init__(self, definitions):
        self.definitions = definitions

    def resolve(self, name, item):
        ref_id = item.get("$ref")
        if ref_id:
            name = ref_id.split("/")[-1]
            item = self.definitions.get(name)
            return self.resolve(name, item)

        return (name, item)

    def visit_property(self, name, prop, debug_prefix=""):
        logger.debug(f"{debug_prefix}Visiting property: {name} {prop}")
        resolved_name, resolved_prop = self.resolve(name, prop)
        logger.debug(
            f"{debug_prefix}Resolved property: {resolved_name} {resolved_prop}"
        )

        resolved_type = get_type(resolved_prop)
        if resolved_type in JSON_SCHEMA_SCALAR_TYPES:
            logger.debug(f"{debug_prefix}Processing scalar type")
            return Field(name, JSON_SCHEMA_TO_MESSAGE_TYPE.get(resolved_type))
        elif resolved_type == "object":
            logger.debug(f"{debug_prefix}Processing object type")
            additional_properties = resolved_prop.get("additionalProperties")
            if additional_properties:
                ap_name, ap = self.resolve(
                    "additionalProperties", additional_properties
                )
                ap_type = get_type(ap)
                if ap_type == "object":
                    return Field(name, f"map<string, {pascal_case(ap_name)}>")
                else:
                    return Field(
                        name, f"map<string, {JSON_SCHEMA_TO_MESSAGE_TYPE.get(ap_type, 'google.protobuf.Any')}>"
                    )
            return Field(name, pascal_case(resolved_name))
        elif resolved_type is None:
            logger.debug(f"{debug_prefix}Processing None type")
            return Field(name, "google.protobuf.Any")
        elif resolved_type == "array":
            logger.debug(f"{debug_prefix}Processing array type")
            items = resolved_prop.get("items")
            logger.debug(f"{debug_prefix}  Visiting items {items}")
            resolved_item_name, resolved_items = self.resolve("items", items)
            logger.debug(
                f"{debug_prefix}  Resolved items {resolved_item_name} {resolved_items}"
            )

            resolved_item_type = get_type(resolved_items)
            if resolved_item_type in JSON_SCHEMA_SCALAR_TYPES:
                logger.debug(f"{debug_prefix}  Processing scalar type")
                return Field(
                    name,
                    JSON_SCHEMA_TO_MESSAGE_TYPE.get(resolved_item_type),
                    rule="repeated",
                )
            elif resolved_item_type == "object":
                logger.debug(f"{debug_prefix}  Processing object type")
                return Field(name, pascal_case(resolved_item_name), rule="repeated")
            elif resolved_item_type is None:
                logger.debug(f"{debug_prefix}  Processing None type")
                return Field(name, "google.protobuf.Any", rule="repeated")
            elif resolved_item_type == "array" and "prefixItems" in resolved_items:
                logger.debug(f"{debug_prefix} Processing prefixItems type")
                return Field(
                    name, "google.protobuf.Any", rule="repeated"
                )  # TODO: can we model this better?

        raise RuntimeError(f"Conversion Error: {resolved_type=} {resolved_item_type=}")

    def visit_definition(self, name, definition):
        properties = definition.get("properties")
        if not properties:
            return None

        return Message(
            pascal_case(name),
            [
                self.visit_property(prop_name, prop_def)
                for prop_name, prop_def in properties.items()
            ],
        )


def convert(contents, prefix=None, package=None):
    lines = []

    if prefix:
        lines.append(prefix)

    lines.append('syntax = "proto3";\n')
    lines.append('import "google/protobuf/any.proto";\n')

    if package:
        lines.append(f"package {package};\n")

    json_contents = json.loads(contents)
    definitions = json_contents.get("definitions", {})
    visitor = Visitor(definitions)

    message = visitor.visit_definition(json_contents.get("title"), json_contents)
    lines.append(str(message))

    for name, definition in definitions.items():
        dtype = get_type(definition)
        if dtype == "object":
            message = visitor.visit_definition(name, definition)
            if message:
                lines.append(str(message))

    return "\n".join(lines)


def main():
    filename = sys.argv[1]
    package = sys.argv[2]

    logging.basicConfig(
        level=logging.DEBUG if os.environ.get("JS2P_DEBUG") else logging.INFO
    )

    with open(filename, "r") as f:
        contents = f.read()

    prefix = f"""// Generated by jsonschema2protobuf.  DO NOT EDIT!
// Source file: {filename}
// Source file sha256 digest: {hashlib.sha256(contents.encode('utf-8')).hexdigest()}
"""

    print(convert(contents, prefix=prefix, package=package))


if __name__ == "__main__":
    main()
