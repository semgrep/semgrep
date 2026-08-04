package main

// -----------------------------------------------------------------------
// Operator folding: multi-hop chains that produce 112 (sink(112) rule)
// -----------------------------------------------------------------------

func testLeftShift() {
	x := 7
	z := x << 4 // 112
	//ruleid: test
	sink(z)
}

func testRightShift() {
	x := 224
	z := x >> 1 // 112
	//ruleid: test
	sink(z)
}

func testBitAnd() {
	x := 255
	z := x & 112 // 112
	//ruleid: test
	sink(z)
}

func testBitOr() {
	x := 96
	z := x | 16 // 112
	//ruleid: test
	sink(z)
}

func testXorChain() {
	// (pad ^ secret) ^ pad = secret = 112
	pad := 204
	secret := 112
	pub := pad ^ secret   // 188
	recovered := pub ^ pad // 112
	//ruleid: test
	sink(recovered)
}

func testBitNot() {
	// ^(-113) = 112  (negative input, positive result).
	// Go does not fold negative integer literals through the IL analysis,
	// so this is a known false negative.
	x := -113
	z := ^x
	//todoruleid: test
	sink(z)
}

func testArithmeticChain() {
	// 3 * 8 + 88 = 112
	a := 3
	b := a * 8  // 24
	c := b + 88 // 112
	//ruleid: test
	sink(c)
}

func testModResult() {
	// 312 % 200 = 112
	x := 312
	z := x % 200
	//ruleid: test
	sink(z)
}

// -----------------------------------------------------------------------
// SCCP: dead-branch elimination (sink("yes") rule)
// -----------------------------------------------------------------------

func testShiftDrivesSccp() {
	// 7 << 4 = 112; `== 112` is always true, else-branch is dead.
	key := 7
	shifted := key << 4
	var result string
	if shifted == 112 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

func testBitAndDrivesSccp() {
	// 5 & 6 = 4; `!= 4` is false, if-branch is dead.
	flags := 5
	mask := 6
	var result string
	if (flags & mask) != 4 {
		result = "no" // dead
	} else {
		result = "yes"
	}
	//proruleid: test-sccp
	sink(result)
}

func testArithmeticChainDrivesSccp() {
	// 3*8 - 16 = 8; `> 5` is true.
	a := 3
	b := a * 8  // 24
	c := b - 16 // 8
	var result string
	if c > 5 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

func testModDrivesSccp() {
	// 7 % 3 = 1; `== 1` is true.
	a := 7
	b := a % 3
	var result string
	if b == 1 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

func testXorDrivesSccp() {
	// 10 ^ 12 = 6; `!= 6` is false, if-branch is dead.
	a := 10
	b := 12
	var result string
	if (a ^ b) != 6 {
		result = "no" // dead
	} else {
		result = "yes"
	}
	//proruleid: test-sccp
	sink(result)
}

func testMultiHopDrivesSccp() {
	// 10*3 - 6 = 24; 24//2 = 12; 12 == 12 is true.
	a := 10
	b := a * 3  // 30
	c := b - 6  // 24
	d := c / 2  // 12
	var result string
	if d == 12 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

// -----------------------------------------------------------------------
// Loop-invariant propagation
// -----------------------------------------------------------------------

func testLoopInvariantInt() {
	// x is never written in the loop; it remains 112 after the loop.
	x := 112
	total := 0
	for i := 0; i < 10; i++ {
		total += i // only total changes
	}
	//proruleid: test
	sink(x)
}

func testLoopInvariantDrivesSccp() {
	// x is defined before the loop and never modified inside it.
	// After the loop x<<4 = 112, and the condition `== 112` drives SCCP.
	x := 7
	total := 0
	for i := 0; i < 10; i++ {
		total += i
	}
	shifted := x << 4 // x=7 survives the loop; 7<<4 = 112
	var result string
	if shifted == 112 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

// Negative-input tests via SCCP: intermediate value is negative, but the
// sink pattern is the string "yes" so the YAML pattern stays positive.

func testNegativeSubtractionDrivesSccp() {
	// 3 - 10 = -7; `-7 != 0` is true.
	a := 3
	b := 10
	diff := a - b // -7
	var result string
	if diff != 0 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

func testNegativeDivisionDrivesSccp() {
	// -8 / 2 = -4 (exact, so folds in the IL); `-4 < 0` is true.
	x := -8
	z := x / 2 // -4
	var result string
	if z < 0 {
		result = "yes"
	} else {
		result = "no" // dead
	}
	//proruleid: test-sccp
	sink(result)
}

// -----------------------------------------------------------------------
// No fold on non-constant inputs
// -----------------------------------------------------------------------

func testNonConstant(n int) {
	z := n & 112
	//ok: test
	sink(z)
}

func testNonConstantSccp(n int) {
	var result string
	if n == 112 {
		result = "yes"
	} else {
		result = "no"
	}
	// OK: n is not constant, both branches are live.
	sink(result)
}
