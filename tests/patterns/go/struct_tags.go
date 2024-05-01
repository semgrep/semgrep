package main 

type Address struct {
    // ERROR:
    Name   string `json:"name"`
    // ERROR:
    Street string `json:"street"`
    // ERROR:
    City   string `json:"sity"`
}

