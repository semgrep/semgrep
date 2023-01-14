package Foo

func request() {
    c := make(chan int)
    for i := 0; i < 5; i++ {
        i := i
	//ERROR: match
        go func() {
            select {
	    case c <- i: foo2()
	    default: foo3()
            }
        }()
    }
}
