package main

// The four cases from https://github.com/semgrep/semgrep/issues/11930.
func test1() {
	var err error
	// ERROR: match
	err = func1()
	err = func2()
}

func test2() {
	var err error
	// ERROR: match
	if err = func1(); err != nil {
	}
	if err = func2(); err != nil {
	}
}

func test3() {
	var err error
	// ERROR: match
	err = func1()
	if err = func2(); err != nil {
	}
}

func test4() {
	var err error
	// ERROR: match
	if err = func1(); err != nil {
	}
	err = func2()
}

func shortDeclarations() {
	// ERROR: match
	if err := func1(); err != nil {
		handle(err)
	}
	if err := func2(); err != nil {
		handle(err)
	}
}

func nestedInitializers() {
	if ready() {
		// ERROR: match
		if err := func1(); err != nil {
			handle(err)
		}
		func2()
	}
}

func multipleReturns() {
	// ERROR: match
	if value, err := func1(); err != nil {
		handle(value, err)
	}
	func2()
}

func ordinaryShortDeclaration() {
	// ERROR: match
	err := func1()
	handle(err)
	func2()
}

func nestedBody() {
	// ERROR: match
	if err := func1(); err != nil {
		func2()
	}
}

func wrongOrder() {
	func2()
	if err := func1(); err != nil {
		handle(err)
	}
}

func differentCall() {
	if err := unrelated(); err != nil {
		handle(err)
	}
	func2()
}

func missingSecondCall() {
	if err := func1(); err != nil {
		handle(err)
	}
	unrelated()
}

func otherFunctionFirst() {
	if err := func1(); err != nil {
		handle(err)
	}
}

func otherFunctionSecond() {
	func2()
}

// Only synthetic initializer blocks are transparent to statement sequences.
func explicitBlock() {
	{
		func1()
	}
	func2()
}

func differentArguments() {
	if err := func1(1); err != nil {
		handle(err)
	}
	func2()
}

// Switch initializers use the same synthetic-block representation.
func switchInitializer() {
	// ERROR: match
	switch err := func1(); err {
	default:
		handle(err)
	}
	func2()
}
