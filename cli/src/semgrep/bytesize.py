#
# Copyright (c) 2021-2024 Semgrep Inc.
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
#
# Parse command-line arguments representing a number of bytes, such as
# '5 mb' or '3.2GiB'.
#
# Maybe we could use an external package for this, such as
# https://pypi.org/project/datasize/ (documentation?)
# or https://github.com/xolox/python-humanfriendly
#
from typing import Optional
from typing import Union

import click

UNITS = {
    "": 1,
    "B": 1,
    "KIB": 2**10,
    "MIB": 2**20,
    "GIB": 2**30,
    "TIB": 2**40,
    "KB": 10**3,
    "MB": 10**6,
    "GB": 10**9,
    "TB": 10**12,
}


def parse_size(input: str) -> int:
    import re

    s = input.upper()
    # note that '1e6' is a valid float and should not become '1 e6'.
    s = re.sub(r"([BKMGT][A-Z]*)", r" \1", s)
    tokens = [sub.strip() for sub in s.split()]
    n = len(tokens)
    if n == 1:
        number = tokens[0]
        unit = ""
    elif n == 2:
        number, unit = tokens
    else:
        raise ValueError(f"Invalid representation for a number of bytes: '{input}'")
    if unit in UNITS:
        return int(float(number) * UNITS[unit])
    else:
        raise ValueError(f"Invalid representation for a number of bytes: '{input}'")


class ByteSizeType(click.ParamType):
    name = "BYTES"

    def convert(
        self,
        value: Union[None, str, int],
        _param: Optional[click.Parameter],
        ctx: Optional[click.Context],
    ) -> Optional[int]:
        try:
            return (
                parse_size(value)
                if isinstance(value, str)
                else value
                if isinstance(value, int)
                else None
            )
        except ValueError as ex:
            raise click.exceptions.UsageError(*ex.args)
