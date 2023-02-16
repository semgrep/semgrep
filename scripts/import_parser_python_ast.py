from __future__ import annotations

import ast

with open("./imports.py") as imports_file:
    contents = imports_file.read()

tree = ast.parse(contents)
print(ast.dump(tree))
try:
    import astpretty

    print("=" * 80)
    astpretty.pprint(tree)
except ImportError:
    print("`astpretty` is not installed. Run pip3 install astpretty for a nicer output")
