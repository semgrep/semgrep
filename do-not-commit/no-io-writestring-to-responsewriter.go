// cf. https://blogtitle.github.io/robn-go-security-pearls-cross-site-scripting-xss/

package main

import (
	"fmt"
	"net/http"
)


func isValid(token string) bool {
	return true
}

func vulnerableHandler(w http.ResponseWriter, r *http.Request) {
  r.ParseForm()
  tok := r.FormValue("token")
  if !isValid(tok) {
    // ruleid:no-io-writestring-to-responsewriter
    io.WriteString(w, fmt.Sprintf("Invalid token: %q", tok))
  }
  // ...
}

// cf. https://github.com/hashicorp/vault-plugin-database-mongodbatlas//blob/9cf156a44f9c8d56fb263f692541e5c7fbab9ab1/vendor/golang.org/x/net/http2/server.go#L2160
func handleHeaderListTooLong(w http.ResponseWriter, r *http.Request) {
	const statusRequestHeaderFieldsTooLarge = 431
	w.WriteHeader(statusRequestHeaderFieldsTooLarge)
  // ok:no-io-writestring-to-responsewriter
	io.WriteString(w, "<h1>HTTP Error 431</h1><p>Request Header Field(s) Too Large</p>")
}
