// simplified from https://sgrep.dev/1nQ
package main

func MyHandler(w http.ResponseWriter, r *http.Request) {
    bar()
    //ERROR: match
    session, err := store.Get(r, "blah-session")
    bar()
    user_id := session.Values["user_id"]
    bar()
    user_id = r.query.params.user_id
    bar()
}
