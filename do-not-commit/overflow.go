package main

import (
	"fmt"
	"strconv"
)

func mainInt16Ex1() {
	// ruleid: interter-overflow-int16
	bigValue, err := strconv.Atoi("2147483648")
	if err != nil {
		panic(err)
	}
	value := int16(bigValue)
	fmt.Println(value)
}

func mainInt16Ex2() {
	// ok
	bigValue, err := strconv.Atoi("10")
	if err != nil {
		panic(err)
	}
	value := int16(bigValue)
	fmt.Println(value)
}

func mainInt32Ex1() {
	// ruleid: interter-overflow-int32
	bigValue, err := strconv.Atoi("2147483648")
	if err != nil {
		panic(err)
	}
	value := int32(bigValue)
	fmt.Println(value)
}

func mainInt32Ex2() {
	// ok
	bigValue, err := strconv.Atoi("10")
	if err != nil {
		panic(err)
	}
	value := int32(bigValue)
	fmt.Println(value)
}

func main() {
	mainInt16Ex1()
	mainInt16Ex2()
	mainInt32Ex1()
	mainInt32Ex2()
}
