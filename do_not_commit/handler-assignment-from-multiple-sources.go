package main
import (
    "net/http"
    "github.com/gorilla/sessions"
)

type User struct {
    user_id int
    account_id string
}


func ValidateUser(user_id int) bool {
    return true
}

func RetrieveUser(user_id int) User {
    return User{user_id, "0000"}
}

var store = sessions.NewCookieStore([]byte("blah-blah-blah"))

func MyHandler(w http.ResponseWriter, r *http.Request) {
    session, err := store.Get(r, "blah-session")
    // ruleid: handler-assignment-from-multiple-sources
    user_id := session.Values["user_id"]

    if !ValidateUser(user_id) {
        http.Error(w, "Error", http.StatusInternalServerError)
        return
    }

    user_id = r.query.params.user_id
    user_obj := RetrieveUser(user_id)
    user_obj.account_id = r.query.params.account_id
    user_obj.save()
}

func MyHandlerExplicit(w http.ResponseWriter, r *http.Request) {
    session, err := store.Get(r, "blah-session")
    // ruleid: handler-assignment-from-multiple-sources
    var user_id int = session.Values["user_id"].(int)

    if !ValidateUser(user_id) {
        http.Error(w, "Error", http.StatusInternalServerError)
        return
    }

    user_id = r.query.params.user_id
    user_obj := RetrieveUser(user_id)
    user_obj.account_id = r.query.params.account_id
    user_obj.save()
}

func augment(user_id int, augment_string string) int {
    return user_id
}

func MyHandlerOK(w http.ResponseWriter, r *http.Request) {
    // ok
    session, err := store.Get(r, "blah-session")
    user_id := session.Values["user_id"]

    if !ValidateUser(user_id) {
        http.Error(w, "Error", http.StatusInternalServerError)
        return
    }

    user_id = augment(user_id, "hello, world")
    user_obj := RetrieveUser(user_id)
    user_obj.account_id = r.query.params.account_id
    user_obj.save()
}

func (sc *http.serverConn) runHandler(rw *http.responseWriter, req *http.Request, handler func(http.ResponseWriter, *http.Request)) {
        // ok
	didPanic := true
	defer func() {
		rw.rws.stream.cancelCtx()
		if didPanic {
			e := recover()
			sc.writeFrameFromHandler(FrameWriteRequest{
				write:  handlerPanicRST{rw.rws.stream.id},
				stream: rw.rws.stream,
			})
			// Same as net/http:
			if e != nil && e != http.ErrAbortHandler {
				const size = 64 << 10
                                // ok
				buf := make([]byte, size)
				buf = buf[:runtime.Stack(buf, false)]
				sc.logf("http2: panic serving %v: %v\n%s", sc.conn.RemoteAddr(), e, buf)
			}
			return
		}
		rw.handlerDone()
	}()
	handler(rw, req)
	didPanic = false
}

func main() {
    http.HandleFunc("/account", MyHandler)

    http.ListenAndServe(":8080", nil)
}

