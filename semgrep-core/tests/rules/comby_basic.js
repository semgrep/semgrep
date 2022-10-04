function foo() {
    //ruleid:
    foo(1, 2)

    //unfortunately it does not match
    foo(1,2)
    //neither this one :(
    foo (1, 2)

    //ruleid: this is detected
    foo(1,
	2)

    //ruleid: this is detectedtoo
    foo(1, // comment
	 2)

    foo(2,1)
}
