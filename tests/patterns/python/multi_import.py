# MATCH:
from z import x, y

# MATCH:
from z import x, a, y

from z import x
from z import y

# MATCH:
from z import a, x, b, y, c
