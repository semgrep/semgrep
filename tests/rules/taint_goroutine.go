package main

func main() {
	value := source()
	//ruleid: test
    go sink(value)
}

func control() {
	value := source()
	//ruleid: test
    sink(value)
}