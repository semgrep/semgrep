# ruleid: python-fstring
test("")

# ruleid: python-fstring
test("asd")

# ruleid: python-fstring
test(f"")

# ruleid: python-fstring
test(f"{2 + 2}")

# ruleid: python-fstring
test(f"jkflajdfkl{3}jdfkakl")

# ruleid: python-fstring
test(f"{f""}{f""}")


# ok: python-fstring
test(f"{input()}")
