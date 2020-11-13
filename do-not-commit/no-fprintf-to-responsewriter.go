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
    // ruleid:no-fprintf-to-responsewriter
    fmt.Fprintf(w, "Invalid token: %q", tok)
  }
  // ...
}

// cf. https://github.com/wrfly/container-web-tty//blob/09f891f0d12d0a930f37b675e2eda5784733579a/route/asset/bindata.go#L242
func dirList(w http.ResponseWriter, r *http.Request, f http.File) {
	dirs, err := f.Readdir(-1)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
    // ok:no-fprintf-to-responsewriter
		fmt.Fprint(w, "Error reading directory")
		return
	}
	sort.Slice(dirs, func(i, j int) bool { return dirs[i].Name() < dirs[j].Name() })
  w.Header().Set("Content-Type", "text/html; charset=utf-8")
  // ok:no-fprintf-to-responsewriter
  fmt.Fprintf(w, "<pre>\n")
  for _, d := range dirs {
    name := d.Name()
    if d.IsDir() {
      name += "/"
    }
    // name may contain '?' or '#', which must be escaped to remain
    // part of the URL path, and not indicate the start of a query
    // string or fragment.
    url := url.URL{Path: filepath.Join(r.RequestURI, name)}
    // ruleid:no-fprintf-to-responsewriter
    fmt.Fprintf(w, "<a href=\"%s\">%s</a>\n", url.String(), d.Name())
  }
  // ok:no-fprintf-to-responsewriter
  fmt.Fprintf(w, "</pre>\n")
}
