func foo() {
	//ERROR: match
        ioutil.WriteFile("foo", []byte{}, 0666)
	//ERROR: match
        ioutil.WriteFile("foo", []byte(data), 0666)
}
