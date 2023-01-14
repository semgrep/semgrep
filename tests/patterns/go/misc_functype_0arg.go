package foo

// this used to crash semgrep with --json with a NoTokenLocation
// exn because the  func() function type didn't have any token.
//ERROR: match
func asd(a func()) {
	a()
}
