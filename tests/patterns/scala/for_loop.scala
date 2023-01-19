object Foo {
    def f() {
        //ERROR:
        for (x <- xs if x % 2 == 0; y <- ys) {
            print(x + y)
        }

        //OK:
        for (x <- xs; y <- ys) {
            print(x + y)
        }
    }
}