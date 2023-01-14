package main

func main() {
    var value interface{}
    // ERROR: 
    _ = v.([]interface{})
}

