# PEP 465 matrix-multiplication operator `@` / `@=` (Python 3.5).

import numpy as np

a = np.array([[1, 2], [3, 4]])
b = np.array([[5, 6], [7, 8]])

# Binary matrix multiplication.
product = a @ b

# Augmented assignment form.
c = a
c @= b

# Chained with other operators (parse precedence).
result = (a @ b) + (b @ a)
