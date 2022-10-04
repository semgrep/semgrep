def foo():
    a = 1,
    # star_expr was handled in most context in the grammar except
    # in return_stmt, where it should be allowed too
    return 2, *a

print()
