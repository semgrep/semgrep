package Foo

import (
	htemplate "html/template"
	ttemplate "text/template"
)

func foo() {
	mainHTemplate()
	mainTTemplate()
}

func mainHTemplate() {
	//ERROR:
	tmpl := htemplate.ParseFiles("index.html")
}

func mainTTemplate() {
	// ok:
	tmpl := ttemplate.ParseFiles("index.html")
}
