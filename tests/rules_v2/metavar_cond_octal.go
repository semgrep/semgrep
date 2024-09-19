package Foo

func foo() {
     os.Mkdir("foo", 0400)
     os.Mkdir("foo", 0600)
     //ruleid: rule_template_id
     os.Mkdir("foo", 0666)
}
