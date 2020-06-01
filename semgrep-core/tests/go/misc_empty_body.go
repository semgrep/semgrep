package Foo;

func foo() {
    if (y == 1) { 
      x = 1
    }
    //ERROR: match
    if (y == 1) {  
    }
}
