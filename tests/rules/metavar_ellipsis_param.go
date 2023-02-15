
// ERROR: match
func foo(int) { }

// ERROR: match
func foo(x int) { }

// ERROR: match
func foo(x, y int) { }

// ERROR: match
func foo(int, string, bool) { }

// ERROR: match
func bar(int, string, bool) { }

func bar(x int, string, bool) { }

func bar(string, string, bool) { }

func bar(string, int, string, bool) { }

func qux() { }

func qux(int) { }

// ERROR: match
func qux(x int) { }

// ERROR: match
func qux(string, x int) { }

func qux(string, x int, string) { }