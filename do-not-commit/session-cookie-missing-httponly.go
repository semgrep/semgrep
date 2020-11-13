// cf. https://github.com/0c34/govwa/blob/139693e56406b5684d2a6ae22c0af90717e149b8/user/session/session.go

package session

import (
	"log"
	"fmt"
	"net/http"
	"govwa/util/config"
	"github.com/gorilla/sessions"
)

type Self struct{}

func New() *Self {
	return &Self{}
}

var store = sessions.NewCookieStore([]byte(config.Cfg.Sessionkey))

func (self *Self) SetSession(w http.ResponseWriter, r *http.Request, data map[string]string) {
	session, err := store.Get(r, "govwa")

	if err != nil {
		log.Println(err.Error())
	}

    // ruleid: session-cookie-missing-httponly
	session.Options = &sessions.Options{
		Path:     "/",
		MaxAge:   3600,
		HttpOnly: false, //set to false for xss :)
        Secure: true,
	}

	session.Values["govwa_session"] = true

	//create new session to store on server side
	if data != nil {
		for key, value := range data {
			session.Values[key] = value
		}
	}
	err = session.Save(r, w) //safe session and send it to client as cookie

		if err != nil {
			log.Println(err.Error())
		}
}

func (self *Self) GetSession(r *http.Request, key string) string  {
	session, err := store.Get(r, "govwa")

	if err != nil {
		log.Println(err.Error())
		return ""
	}
	data := session.Values[key]
	sv := fmt.Sprintf("%v", data)
	return sv
}

func (self *Self) DeleteSession(w http.ResponseWriter, r *http.Request) {
	session, err := store.Get(r, "govwa")
	if err != nil {
		log.Println(err.Error())
	}

    // ruleid: session-cookie-missing-httponly
	session.Options = &sessions.Options{
		MaxAge:   -1,
		HttpOnly: false, //set to false for xss :)
	}

	session.Values["govwa_session"] = false
	err = session.Save(r, w) //safe session and send it to client as cookie

	if err != nil {
		log.Println(err.Error())
	}

	return
}

func (self *Self) IsLoggedIn(r *http.Request) bool {
	s, err := store.Get(r, "govwa")
	if err != nil {
		log.Println(err.Error())
	}
	if auth, ok := s.Values["govwa_session"].(bool); !ok || !auth {
		return false
	}
	return true
}
