package main

func main() {

     a = "foo"
     a = "don't do that"
     var quoteEscaper = strings.NewReplacer(
       "\\",
       "\\\\",
       `"`,
       "\\\"")
}
