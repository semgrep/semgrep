package Foo

func foo() {
   //ERROR:
    myfile = open()
    close(myfile)
}
