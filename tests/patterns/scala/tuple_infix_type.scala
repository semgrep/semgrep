object Foo {
    //OK:
    def f[S] : S -| S = stuff
    //ERROR:
    def g[S] : (S,*) -| (S => *) = other_stuff
}