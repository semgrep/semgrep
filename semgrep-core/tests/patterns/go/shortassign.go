// https://github.com/returntocorp/semgrep/issues/2452
package foo

import "fmt"

func f(x func ()) {
	go x()
}

func foo1(ch chan int) {
	// ERROR:
	for i := range ch {
		f(func() {
			fmt.Println(i)
		})
	}
}

func foo2(ch chan int) {
	// OK:
	for i := range ch {
		// copy i into a loop-body-local variable so that the closure
		// captures a reference to it instead of the loop variable
		i := i // this declares a new variable !
		f(func() {
			fmt.Println(i)
		})
	}
}
