import sys
import re

RE_LINE = re.compile(r"( +)var f(\$\d+)? = \[0, ftype, (\d+), fname\];$")

MAPPING = {
    0: 0,
    None: 4,
    4: 4,
    3: 28,
    2: 16,
    1: 0,
    6: 0,
    5: 4,
    10: 0,
    9: 4,
    8: 8,
    7: 16,
    13: 8,
    12: 0,
    11: 4,
    17: 4,
    16: 44,
    15: 32,
    14: 0,
    19: 0,
    18: 4,
    20: 0,
    23: 4,
    22: 8,
    21: 0,
    25: 4,
    24: 0,
    26: 0,
    30: 0,
    29: 8,
    28: 12,
    27: 4,
    37: 0,
    36: 12,
    35: 16,
    34: 20,
    33: 24,
    32: 4,
    31: 8,
    41: 0,
    40: 8,
    39: 12,
    38: 4,
    44: 12,
    43: 4,
    42: 0,
    45: 0
}

fp = sys.stdin

if len(sys.argv) > 1:
    fp = open(sys.argv[1])

for line in fp:
    match = RE_LINE.match(line)
    if not match:
        sys.stdout.write(line)
        continue
    
    whitespace, suffix, value = match.groups()

    index = None
    if suffix:
        index = int(suffix[1:])
    else:
        suffix = ""
    
    sys.stdout.write(f"{whitespace}var f{suffix} = [0, ftype, {MAPPING.get(index, value)}, fname];\n")
