package main

import (
    "crypto/tls"
    "encoding/json"
    "encoding/hex"
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func (s *server) handlerBadFmt (w http.ResponseWriter, r *http.Request) {
    urls, ok := r.URL.Query()["url"] // extract url from query params

    if !ok {
		http.Error(w, "url missing", 500)
		return
	}

	if len(urls) != 1 {
		http.Error(w, "url missing", 500)
		return
	}

    url := fmt.Sprintf("//%s/path", urls[0])

    // ruleid: test
    resp, err := http.Get(url) // sink
    if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

    // ok: test
	_, err3 := http.Get("https://semgrep.dev")
	if err3 != nil {
		http.Error(w, err.Error(), 500)
		return
    }

    url4 := fmt.Sprintf("ftps://%s/path/to/%s", "test", r.URL.Path)
    // ok: test
	_, err4 := http.Get("https://semgrep.dev")
	if err3 != nil {
		http.Error(w, err.Error(), 500)
		return
	}

    defer resp.Body.Close()

    bytes, err := ioutil.ReadAll(resp.Body)
    if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

    // Write out the hexdump of the bytes as plaintext.
	w.Header().Set("Content-Type", "text/plain; charset=utf-8")
	fmt.Fprint(w, hex.Dump(bytes))
}
