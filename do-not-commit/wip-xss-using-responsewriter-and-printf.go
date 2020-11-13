package main

import (
	"fmt"
	"log"
	"net/http"
)

func getMovieQuote() map[string]string {
	m := make(map[string]string)
	m["quote"] = "I'll be back."
	m["movie"] = "The Terminator"
	m["year"] = "1984"

	return m
}

func indexPage(w http.ResponseWriter, r *http.Request) {
    const tme = `<html>`

	const template = `
	<html>
	<body>
	  <h1>Random Movie Quotes</h1>
	  <h2>%s</h2>
	  <h4>~%s, %s</h4>
	</body>
	</html>`

	quote := getMovieQuote()

	quoteText := quote["quote"]
	movie := quote["movie"]
	year := quote["year"]

	w.WriteHeader(http.StatusAccepted)
	w.Write([]byte(fmt.Sprintf(template, quoteText, movie, year)))
}

func errorPage(w http.ResponseWriter, r *http.Request) {
    // ruleid: wip-xss-using-responsewriter-and-printf
	params := r.URL.Query()
	urls, ok := params["url"]
	if !ok {
		log.Println("Error")
		return
	}
	url := urls[0]

    const template = `
	<html>
	<body>
	  <h1>error; page not found. <a href="%s">go back</a></h1>
	</body>
	</html>`

	w.WriteHeader(http.StatusAccepted)
	w.Write([]byte(fmt.Sprintf(template, url)))
}

func main() {
	http.HandleFunc("/", indexPage)
	http.HandleFunc("/error", errorPage)
	http.ListenAndServe(":8080", nil)
}
