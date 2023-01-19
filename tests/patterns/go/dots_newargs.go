package Foo

func bad() {

 //ERROR: match
 req := &http.Request {
        Method: "POST",
        URL: reqURL,
        Header: map[string][]string {
            "Content-Type": { "application/json; charset=UTF-8" },
        },
        Body: reqBody,
    }

}
