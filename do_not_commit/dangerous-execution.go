package blah

import (
    "net/http"
    "github.com/robertkrimen/otto"
)

func whyyyy(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		panic(err)
	}
	script := r.Form.Get("script")

    vm := otto.New()

    // ruleid: dangerous-execution
    vm.Run(script)
}

func main() {
    vm := otto.New()
    // ok
    vm.Run(`
        abc = 2 + 2;
        console.log("The value of abc is " + abc); // 4
    `)
}