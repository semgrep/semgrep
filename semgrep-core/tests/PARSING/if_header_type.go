package main

import (
	"fmt"
	"github.com/google/go-cmp/cmp"
)

func main() {

	var a map[string]map[string]uint64

	if want, have := map[string]map[string]uint64{
		"a": {"x": 1},
		"b": {"x": 1},
	}, a; !cmp.Equal(want, have) {
		fmt.Println(cmp.Diff(want, have))
	}

}
