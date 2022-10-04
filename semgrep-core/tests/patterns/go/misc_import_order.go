package main

//ERROR: match
import (
    htemplate "html/template"
    ttemplate "text/template"
)

func Whatever() {
    return htemplate.Exec()
}

func main() {}
