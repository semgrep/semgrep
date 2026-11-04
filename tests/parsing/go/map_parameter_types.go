package parameters

func consume([]string, int, map[int]int, error) {}
func produce() ([]string, int, map[int]int, error)
func roundtrip([]string, int, map[int]int, error) ([]string, int, map[int]int, error)
func named(x, y int, rest ...string) { consume(nil, x, nil, nil); named(x, y, rest...) }
var callback func([]string, map[string]map[int]int) (map[string]int, error)
