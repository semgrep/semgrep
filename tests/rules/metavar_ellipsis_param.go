
// ruleid: metavar_ellipsis_param
func foo(int) { }

// ruleid: metavar_ellipsis_param
func foo(x int) { }

// ruleid: metavar_ellipsis_param
func foo(x, y int) { }

// ruleid: metavar_ellipsis_param
func foo(int, string, bool) { }

// ruleid: metavar_ellipsis_param
func bar(int, string, bool) { }

func bar(x int, string, bool) { }

func bar(string, string, bool) { }

func bar(string, int, string, bool) { }

func qux() { }

func qux(int) { }

// ruleid: metavar_ellipsis_param
func qux(x int) { }

// ruleid: metavar_ellipsis_param
func qux(string, x int) { }

func qux(string, x int, string) { }