module 0xcafe::concrete_syntax {
    fun foo(x: u64, y: u64): u64 {
        x * y
    }

    fun main() {
    	//ERROR: match
        let result = foo(1,2);
	    print(result);
    }
}
