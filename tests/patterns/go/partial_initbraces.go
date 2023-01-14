package Foo


type A struct {
    OK int
    Deprecated int
}

func something() []*A {
    return []*A {
	    //ERROR: match
        {
            OK: 5,
        },
	    //ERROR: match
        {
            OK: 6,
        },
	    //ERROR: match
        {
            OK: 7,
            Deprecated: 8,
        },
    }
}