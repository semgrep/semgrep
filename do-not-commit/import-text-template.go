// cf. https://www.veracode.com/blog/secure-development/use-golang-these-mistakes-could-compromise-your-apps-security

package main

// ruleid: import-text-template
import (
	"net/http"
  "text/template"
  "encoding/json"
  "io/ioutil"
  "os"
)

const tmpl = ""

type TodoPageData struct {
  PageTitle string
  Todos []Todo
}

type Todo struct {
  Title string "json:title"
  Done bool "json:done"
}

func (t Todo) ToString() string {
  bytes, _ := json.Marshal(t)
  return string(bytes)
}

func getTodos() []Todo {
  todos := make([]Todo, 3)
  raw, _ := ioutil.ReadFile("./todos.json")
  json.Unmarshal(raw, &todos)
  return todos

}

func main() {
  tmpl := template.Must(template.ParseFiles("index.html"))

  http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
      data := TodoPageData {
          PageTitle: "My Todos!",
          Todos: getTodos(),
      }

      tmpl.Execute(w, data)

  })

  http.ListenAndServe(":" + os.Getenv("PORT"), nil)
}
