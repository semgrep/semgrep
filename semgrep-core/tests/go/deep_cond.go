package main

func foo() {

    //ERROR: match
    if x == true {
      return 1
    }

    //ERROR: match
    if (b == c) && (x == true) {
      return 1
    }
}