package parameters

// ERROR: match
func consume([]string, int, map[int]int, error) {}
// ERROR: match
func process([]string, int, map[int]int, error) { first(); second() }
func differentKey([]string, int, map[string]int, error) {}
func differentOrder([]string, map[int]int, int, error) {}
