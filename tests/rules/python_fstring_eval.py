# ok: python-fstring
test("")

# ok: python-fstring
test("asd")

# ok: python-fstring
test(f"")

# ok: python-fstring
test(f"{2 + 2}")

# ok: python-fstring
test(f"jkflajdfkl{3}jdfkakl")

# ok: python-fstring
test(f"{f""}{f""}")


# ok: python-fstring
test(f"{input()}")


# ruleid: python-fstring
test("h3110 world!")

# ruleid: python-fstring
test(f"h{3110} world!")

# ok: python-fstring
test(f"h{3110.} world!")


# ruleid: python-fstring
test("3.14")

# ruleid: python-fstring
test(f"3.14")


# ruleid: python-fstring
test(f"{3.140}")

pi = 3.14
# ruleid: python-fstring
test(f"{pi}")

# todo: python-fstring
test(f"{3.1415:.2f}")
