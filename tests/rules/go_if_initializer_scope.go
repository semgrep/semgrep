package main

func initializerBody() {
	if err := dangerousCall(); err != nil {
		// ruleid: go-if-initializer-scope
		handle(err)
	}
}

func ordinaryDeclaration() {
	err := dangerousCall()
	// ruleid: go-if-initializer-scope
	handle(err)
}

func shadowedOuter(err error) {
	if err := dangerousCall(); err != nil {
		// ruleid: go-if-initializer-scope
		handle(err)
	}
	// ok: go-if-initializer-scope
	handle(err)
}

func siblingInitializers() {
	if err := dangerousCall(); err != nil {
		// ruleid: go-if-initializer-scope
		handle(err)
	}
	if err := unrelated(); err != nil {
		// ok: go-if-initializer-scope
		handle(err)
	}
}

func nestedShadowing() {
	if err := dangerousCall(); err != nil {
		if err := unrelated(); err != nil {
			// ok: go-if-initializer-scope
			handle(err)
		}
		// ruleid: go-if-initializer-scope
		handle(err)
	}
}

func elseBranch() {
	if err := dangerousCall(); err != nil {
		unrelated()
	} else {
		// ruleid: go-if-initializer-scope
		handle(err)
	}
}

func reassignment(err error) {
	if err = dangerousCall(); err != nil {
		// ok: go-if-initializer-scope
		handle(err)
	}
}

func multipleReturnsShadowed(err error) {
	if value, err := dangerousPair(); err != nil {
		use(value)
		// ruleid: go-if-initializer-scope
		handle(err)
	}
	// ok: go-if-initializer-scope
	handle(err)
}

func nestedMultipleReturns() {
	if err := dangerousCall(); err != nil {
		if value, err := unrelatedPair(); err != nil {
			use(value)
			// ok: go-if-initializer-scope
			handle(err)
		}
		// ruleid: go-if-initializer-scope
		handle(err)
	}
}

func noDeepOrdinary() {
	// ruleid: go-if-initializer-no-deep
	func1()
	func2()
}

func noDeepInitializer() {
	// ok: go-if-initializer-no-deep
	if err := func1(); err != nil {
		handle(err)
	}
	func2()
}

func multipleReturnRedeclaration() {
	err := unrelated()
	value, err := dangerousPair()
	use(value)
	// ruleid: go-if-initializer-scope
	handle(err)
}

func blankIdentifier() {
	if _, err := dangerousPair(); err != nil {
		// ruleid: go-if-initializer-scope
		handle(err)
	}
}

func switchShadowing(err error) {
	switch err := dangerousCall(); err {
	default:
		// ruleid: go-if-initializer-scope
		handle(err)
	}
	// ok: go-if-initializer-scope
	handle(err)
}
