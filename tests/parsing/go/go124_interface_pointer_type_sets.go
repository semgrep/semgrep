package go124

type PointerToNamed interface { *int }
type PointerToStruct interface { *struct{ A int } }
type ApproxPointer interface { ~*int }
type ApproxPointerToStruct interface { ~*struct{ A int } }
type NestedPointer interface { **struct{ A int } | ~[]byte }
type ArrayOrPointer interface { ~[2]int | *int }
type OnlyFunction interface { ~func(int) int }
type OnlyChannel interface { ~chan int }
type OnlyMap interface { ~map[string]int }
type OnlySlice interface { ~[]byte }

func stillScannedToo(xs []int) {
    for range xs { println("observed") }
}
