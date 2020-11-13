// cf. https://github.com/0c34/govwa/blob/139693e56406b5684d2a6ae22c0af90717e149b8/util/cookie.go

package util

import (
	"net/http"
	"time"
)

func SetCookieLevel(w http.ResponseWriter, r *http.Request, cookievalue string){

	level := cookievalue
	if level == "" {
		level = "low"
	}
	SetCookie(w,"Level",level)

}

func CheckLevel(r *http.Request) bool {
	level := GetCookie(r, "Level")
	if level == "" || level == "low" {
		return false //set default level to low
	} else if level == "high" {
		return true //level == high
	} else {
		return false // level == low
	}
}

/* cookie setter getter */

func SetCookie(w http.ResponseWriter, name, value string){
    // ruleid: cookie-missing-secure
	cookie := http.Cookie{
		Name: name,
		Value: value,
	}
	http.SetCookie(w, &cookie)
}

func SetSecureCookie(w http.ResponseWriter, name, value string){
    // ok: cookie-missing-secure
	cookie := http.Cookie{
        Secure: true,
        HttpOnly: true,
		Name: name,
		Value: value,
	}
	http.SetCookie(w, &cookie)
}

func GetCookie(r *http.Request, name string)string{
	cookie, _ := r.Cookie(name)
	return cookie.Value
}

func DeleteCookie(w http.ResponseWriter, cookies []string){
	for _,name := range cookies{
        // ruleid: cookie-missing-secure
		cookie := &http.Cookie{
			Name:     name,
			Value:    "",
			Expires: time.Unix(0, 0),
		}
		http.SetCookie(w, cookie)
	}
}
