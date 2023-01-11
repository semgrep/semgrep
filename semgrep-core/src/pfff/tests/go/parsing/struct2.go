package main

func main() {
     type Address struct {
       name string
       street string
       city string
     }
     // ERROR: should match
     var a = Address { name: "hello", street: "what", city: "SF"}

     // ERROR: should match
     var b = Address { street: "what", name: "hello", city: "SF"}

     // should not match
     var c = Address { street: "hello", name: "what", city: "SF"}
 }
