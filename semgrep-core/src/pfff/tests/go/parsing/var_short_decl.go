package Foo;
// :=

func testFileLevelDict(t *testing.T, fn string) {
	// Read the file, as golden output.
 	golden, err := os.Open(fn)
	if err != nil {
		t.Errorf("%s (level=%d, dict=%q): %v", fn)
		return
	}
}
