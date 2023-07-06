func DeleteHandlerOk(db *sql.DB, dbutil *util.DB) func(w http.ResponseWriter, req *http.Request) {
    return func(w http.ResponseWriter, req *http.Request) {
        del := req.URL.Query().Get("del")
        idhtml := req.URL.Query().Get("Id")
        if del == "del" {
            // ruleid: no-direct-db-exec
            _, err = db.Exec("DELETE FROM table WHERE Id = " + idhtml)
            // ok: no-direct-db-exec
            _, err = dbutil.Exec("DELETE FROM table WHERE Id = " + idhtml)
            if err != nil {
                panic(err)
            }
        }
    }
}
