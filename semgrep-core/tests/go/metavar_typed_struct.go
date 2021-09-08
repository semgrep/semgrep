func foo() {
	var a http.Server
	//Error: match
	a.ListenAndServe()

	b := http.Server{}
	//Error: match
	b.ListenAndServe()

	var c = http.Server{}
	//Error: match
	c.ListenAndServe()

	d := http.Server{Addr:":8080"}
	//Error: match
	d.ListenAndServe()

	var e = http.Server{Addr:":8080",ReadTimeout:10}
	//Error: match
	e.ListenAndServe()
}