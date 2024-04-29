package main 

type Address struct {
    // ERROR:
    Name   string `json:"name"`
    Street string `json:"street"`
    City   string `json:"sity"`
}

