package Foo

// If you dump this file, you could have seen before
// an hidden Empty stmt added at the end of a Block.
// sgrep did not like that.

// was not generating extra Empty
func bar1() { bar() }

// was generating extra Empty
func bar2() { bar(); }

// was generating extra Empty
func bar3() {
     bar();
}
