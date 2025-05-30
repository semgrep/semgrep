const groups = 2

// ruleid: int-binop
test(2 + groups)

// ruleid: int-binop
test(2 * groups)

// ruleid: int-binop
test(2 % groups)


const pi = 3.14

// ok: int-binop
test(2 + pi)

// ok: int-binop
test(2 * pi)

// ok: int-binop
test(x * x)

// ok: int-binop
test("jkalfd")




