package Foo

const Bar = "password"

func foo() {
     //ERROR: match!
     dangerous1("password");

     //ERROR: match!
     dangerous2(Bar);
}
