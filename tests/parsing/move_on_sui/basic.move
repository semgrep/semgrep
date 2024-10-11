module xxx {
    fun foo(x: u64, y: u64): u64 {
        x * y
    }

    fun main() {
        let result = foo(1,2);
	print(result);
    }
}
