package main

import "fmt"
import "html/template"

func main() {
    var g = "foo"

    // ruleid:go-insecure-templates
    const a template.HTML = fmt.Sprintf("<a href=%q>link</a>")
    // ruleid:go-insecure-templates
    var b template.CSS = "a { text-decoration: underline; } "
    
    // ruleid:go-insecure-templates
    var c template.HTMLAttr =  fmt.Sprintf("herf=%q")

    // ruleid:go-insecure-templates
    const d template.JS = "{foo: 'bar'}"

    // ruleid:go-insecure-templates
    var e template.JSStr = "setTimeout('alert()')";

    // ruleid:go-insecure-templates
    var f template.Srcset = g;
    
    // ok
    tmpl, err := template.New("test").ParseFiles("file.txt")

    // other code
    myTpl.Execute(w, a);
}
