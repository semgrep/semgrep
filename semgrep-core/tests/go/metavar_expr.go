package main

func main() {
    var value interface{}
    // ERROR: 
    _ = v.([]interface{})

	var a map[string]map[string]uint64

    // ERROR: 
	if want, have := map[string]map[string]uint64{
        // ERROR: 
		"a": {"x": 1},
        // ERROR: 
		"b": {"x": 1},
    // ERROR: 
	}, a; !cmp.Equal(want, have) {
        // ERROR: 
		fmt.Println(cmp.Diff(want, have))
	}
}

