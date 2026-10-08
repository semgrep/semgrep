package instantiation

func f() {
	// ERROR: match
	consume(Identity[[]int])
	// ERROR: match
	consume(other.Pair[int, string])
	ignore(other.Pair[int, string])
}
