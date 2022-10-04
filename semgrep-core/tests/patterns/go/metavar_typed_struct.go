func foo() {
	var a http.Server
	//ERROR: match
	a.ListenAndServe()

	b := http.Server{}
	//ERROR: match
	b.ListenAndServe()

	var c = http.Server{}
	//ERROR: match
	c.ListenAndServe()

	d := http.Server{Addr:":8080"}
	//ERROR: match
	d.ListenAndServe()

	var e = http.Server{Addr:":8080",ReadTimeout:10}
	//ERROR: match
	e.ListenAndServe()

	var f = http.Client{}
	//OK:
	f.ListenAndServe()
}