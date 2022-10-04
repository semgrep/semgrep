package Foo

func foo() {
     os.Mkdir("foo", 0400)
     os.Mkdir("foo", 0600)
     //ruleid: match
     os.Mkdir("foo", 0666)
}
