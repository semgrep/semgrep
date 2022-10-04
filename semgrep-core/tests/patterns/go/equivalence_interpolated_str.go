package main


func ConcatInline(r *http.Request) template.HTML {
		customerId := r.URL.Query().Get("id")
		
    // ERROR:
    return template.HTML("<html><body><h1>" + customerId + "</h1></body></html>")
}